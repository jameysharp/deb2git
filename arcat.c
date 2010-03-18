#include <archive.h>
#include <archive_entry.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define die(message, args...) do { fprintf(stderr, message "\n", ##args); exit(EXIT_FAILURE); } while(0)

struct node {
	struct node *link[2];
	mode_t filetype;
	char pathname[0];
};

static int is_duplicate(struct node **seen, const char *pathname, mode_t filetype)
{
	struct node *cur = *seen;
	struct node *prev = NULL;
	int direction = cur && strcmp(pathname, cur->pathname) > 0;
	for(; cur; prev = cur, cur = cur->link[direction])
	{
		int cmp = strcmp(pathname, cur->pathname);
		if(cmp == 0)
		{
			if(cur->filetype != AE_IFDIR || filetype != AE_IFDIR)
				die("file \"%s\" overwritten by later archive entry", pathname);
			*seen = cur;
			return 1;
		}
		if((cmp < 0) == direction)
			break;
	}
	/* at this point, either the list is empty or the loop has updated prev. */
	int len = strlen(pathname) + 1;
	struct node *new = malloc(sizeof(struct node) + len);
	if(!new)
		die("malloc failed for path \"%s\"", pathname);
	new->link[direction] = cur;
	new->link[!direction] = prev;
	new->filetype = filetype;
	memcpy(new->pathname, pathname, len);
	if(prev)
		prev->link[direction] = new;
	if(cur)
		cur->link[!direction] = new;
	*seen = new;
	return 0;
}

static void copy_archive(const char *filename, struct archive *to, struct node **seen)
{
	struct archive_entry *entry;
	struct archive *from = archive_read_new();

	archive_read_support_compression_all(from);
	archive_read_support_format_all(from);

	if(archive_read_open_filename(from, filename, 16384) != ARCHIVE_OK)
		die("archive_read_open %s: %s", filename, archive_error_string(from));

	while(archive_read_next_header(from, &entry) == ARCHIVE_OK)
	{
		char buf[4096];
		ssize_t length;
		if(is_duplicate(seen, archive_entry_pathname(entry), archive_entry_filetype(entry)))
			continue;
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

	archive_read_finish(from);
}

int main(int argc, char **argv)
{
	struct node *seen = 0;
	struct archive *out = archive_write_new();
	archive_write_set_compression_gzip(out);
	archive_write_set_format_cpio_newc(out);
	archive_write_open_filename(out, NULL);

	if(argc <= 1)
		copy_archive(NULL, out, &seen);
	else
		while(argc-- > 1)
			copy_archive(argv++[1], out, &seen);

	archive_write_finish(out);
	exit(EXIT_SUCCESS);
}
