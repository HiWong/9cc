// configurations for Mac OS X
#include <unistd.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

static char template[] = "/tmp/mcc.temp.XXXXXXXXXX";

char *mk_temp_dir()
{
    return mkdtemp(template);
}

int file_exists(const char *path)
{
    struct stat st;
    return stat(path, &st) == 0;
}

int callsys(const char *path, char *argv[])
{
    pid_t pid;
    int ret = EXIT_SUCCESS;
    pid = fork();
    if (pid == 0) {
	// child process
	execv(path, argv);
        fprintf(stderr, "%s: %s\n", strerror(errno), path);
	exit(EXIT_FAILURE);
    }
    else if (pid > 0) {
	// wait for
	int status;
	int retpid = waitpid(pid, &status, 0);
        if (retpid != pid || !WIFEXITED(status) || WEXITSTATUS(status) != 0) {
	    ret = EXIT_FAILURE;
	}
    }
    else {
	perror("Can't fork");
	ret = EXIT_FAILURE;
    }

    return ret;
}
