#include <archive.h>
#include <archive_entry.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define die(message, args...) do { fprintf(stderr, message "\n", ##args); exit(EXIT_FAILURE); } while(0)

static void skip_file(struct archive *a, const char *expected_pathname)
{
	struct archive_entry *entry;
	archive_read_next_header(a, &entry);
	if(strcmp(expected_pathname, archive_entry_pathname(entry)))
		die("not a Debian binary archive: missing %s entry", expected_pathname);
	archive_read_data_skip(a);
}

static void copy_archive(struct archive *from, struct archive *to)
{
	struct archive_entry *entry;
	while(archive_read_next_header(from, &entry) == ARCHIVE_OK)
	{
		char buf[4096];
		ssize_t length;
		archive_write_header(to, entry);
		while((length = archive_read_data(from, buf, sizeof buf)) > 0)
		{
			char *p = buf;
			while(length > 0)
			{
				ssize_t n = archive_write_data(to, p, length);
				if(n <= 0)
					die("archive_write_data: %s", archive_error_string(to));
				length -= n;
				p += n;
			}
		}
		if(length < 0)
			die("archive_read_data: %s", archive_error_string(from));
	}
}

static int recursive_noop(struct archive *a, void *client_data)
{
	return ARCHIVE_OK;
}

static ssize_t recursive_read(struct archive *a, void *client_data, const void **buf)
{
	struct archive *base = client_data;
	size_t length;
	off_t offset;
	int ret = archive_read_data_block(base, buf, &length, &offset);
	if(ret == ARCHIVE_OK)
		return length;
	archive_copy_error(a, base);
	return ret;
}

static void copy_deb(const char *filename, struct archive *dest)
{
	struct archive_entry *entry;
	struct archive *data;
	struct archive *deb = archive_read_new();

	archive_read_support_format_ar(deb);

	if(archive_read_open_filename(deb, filename, 16384) != ARCHIVE_OK)
		die("archive_read_open %s: %s", filename, archive_error_string(deb));

	skip_file(deb, "debian-binary");
	skip_file(deb, "control.tar.gz");
	if(archive_read_next_header(deb, &entry) != ARCHIVE_OK)
		die("data section is missing");

	data = archive_read_new();
	archive_read_support_compression_all(data);
	archive_read_support_format_all(data);
	if(archive_read_open(data, deb, recursive_noop, recursive_read, recursive_noop) != ARCHIVE_OK)
		die("archive_read_open data section: %s", archive_error_string(deb));

	copy_archive(data, dest);

	archive_read_finish(data);
	archive_read_finish(deb);
}

int main(int argc, char **argv)
{
	struct archive *out = archive_write_new();
	archive_write_set_compression_gzip(out);
	archive_write_set_format_cpio(out);
	archive_write_open_filename(out, NULL);

	if(argc <= 1)
		copy_deb(NULL, out);
	else
		while(argc-- > 1)
			copy_deb(argv++[1], out);

	archive_write_finish(out);
	exit(EXIT_SUCCESS);
}
