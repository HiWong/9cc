#include "cc.h"

static struct vector *files;
static const char *original;
struct file *current_file;

enum {
    FILE_KIND_REGULAR = 1,
    FILE_KIND_STRING,
};

bool is_original_file(const char *file)
{
    if (original && !strcmp(original, file))
        return true;
    else
        return false;
}

static void warning_no_newline(const char *file)
{
    if (is_original_file(file))
        fprintf(stderr,
                CLEAR "%s: " RESET PURPLE("warning: ")
                "No newline at end of file\n",
                file);
}

static struct file *new_file(void)
{
    /**
     * NOTE:
     * If it's a temp buffer:
     * 1. _NOT_ allocate buf (save memory)
     * 2. _NOT_ add newline at the end
     * Otherwise the buffer will generate an
     * additional newline when expanding a macro.
     */
    struct file *fs = zmalloc(sizeof(struct file));
    fs->line = 1;
    fs->column = 0;
    fs->bol = true;
    fs->ifstubs = vec_new();
    fs->buffer = vec_new();
    fs->tokens = vec_new();
    return fs;
}

static struct file *open_regular(const char *file)
{
    struct file *fs = new_file();
    fs->kind = FILE_KIND_REGULAR;
    FILE *fp = fopen(file, "r");
    if (fp == NULL)
        die("%s: %s", file, strerror(errno));
    fs->fp = fp;
    fs->file = file;
    // read the content
    fseek(fp, 0, SEEK_END);
    long size = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    fs->buf = xmalloc(size + 2);
    fread(fs->buf, size, 1, fp);
    fs->pc = fs->buf;
    /**
     * Add a newline character to the end if the
     * file doesn't have one, thus the include
     * directive would work well.
     */
    fs->buf[size] = '\n';
    fs->buf[size + 1] = '\n';
    if (fs->buf[size - 1] != '\n') {
        warning_no_newline(file);
        fs->pe = &fs->buf[size + 1];
    } else {
        fs->pe = &fs->buf[size];
    }
    return fs;
}

static struct file *open_string(const char *string)
{
    struct file *fs = new_file();
    size_t len = strlen(string);
    fs->kind = FILE_KIND_STRING;
    fs->file = "<anonymous-string>";
    fs->buf = xstrdup(string);
    fs->buf[len] = '\n';
    fs->pc = fs->buf;
    fs->pe = &fs->buf[len];
    return fs;
}

static void close_file(struct file *fs)
{
    if (fs->kind == FILE_KIND_REGULAR)
        fclose(fs->fp);
    free(fs->buf);
    free(fs);
    // reset current 'bol'
    struct file *current = current_file;
    if (current)
        current->bol = true;
}

void file_sentinel(struct file *fs)
{
    vec_push(files, fs);
    current_file = fs;
}

void file_unsentinel(void)
{
    struct file *fs = vec_pop(files);
    current_file = vec_tail(files);
    close_file(fs);
}

void file_stub(struct file *fs)
{
    fs->stub = true;
    file_sentinel(fs);
}

void file_unstub(void)
{
    file_unsentinel();
}

struct file *with_string(const char *input, const char *name)
{
    struct file *fs = open_string(input);
    fs->name = name ? name : "<anonymous-string>";
    return fs;
}

struct file *with_file(const char *file, const char *name)
{
    struct file *fs = open_regular(file);
    fs->name = name ? name : "<anonymous-file>";
    return fs;
}

struct file *with_buffer(struct vector *v)
{
    struct file *fs = new_file();
    fs->kind = FILE_KIND_STRING;
    fs->name = current_file->name;
    fs->line = current_file->line;
    fs->column = current_file->column;
    vec_add(fs->buffer, v);
    return fs;
}

struct ifstub *new_ifstub(struct ifstub *i)
{
    struct ifstub *ic = zmalloc(sizeof(struct ifstub));
    memcpy(ic, i, sizeof(struct ifstub));
    return ic;
}

void if_sentinel(struct ifstub *i)
{
    vec_push(current_file->ifstubs, i);
}

void if_unsentinel(void)
{
    vec_pop(current_file->ifstubs);
}

struct ifstub *current_ifstub(void)
{
    return vec_tail(current_file->ifstubs);
}

void input_init(const char *file)
{
    original = file;
    files = vec_new();
    file_sentinel(with_file(file, file));
}
