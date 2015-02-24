/* notify.c -- Module to notify of new mail
 *
 * Copyright (c) 1994-2008 Carnegie Mellon University.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The name "Carnegie Mellon University" must not be used to
 *    endorse or promote products derived from this software without
 *    prior written permission. For permission or any legal
 *    details, please contact
 *      Carnegie Mellon University
 *      Center for Technology Transfer and Enterprise Creation
 *      4615 Forbes Avenue
 *      Suite 302
 *      Pittsburgh, PA  15213
 *      (412) 268-7393, fax: (412) 268-7395
 *      innovation@andrew.cmu.edu
 *
 * 4. Redistributions of any form whatsoever must retain the following
 *    acknowledgment:
 *    "This product includes software developed by Computing Services
 *     at Carnegie Mellon University (http://www.cmu.edu/computing/)."
 *
 * CARNEGIE MELLON UNIVERSITY DISCLAIMS ALL WARRANTIES WITH REGARD TO
 * THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS, IN NO EVENT SHALL CARNEGIE MELLON UNIVERSITY BE LIABLE
 * FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING
 * OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/un.h>
#include <unistd.h>

#define IMAPOPT_NOTIFYSOCKET 0
#define FNAME_NOTIFY_SOCK "/egara-notify"
#define EXPORTED

#define NOTIFY_MAXSIZE 8192

const int LOG_ERR = 0;
const char *config_dir = "/tmp";

#define xclose(fd) \
    do { \
	int *_fdp = &(fd); \
	if (*_fdp >= 0) { \
	    close(*_fdp); \
	    *_fdp = -1; \
	} \
    } while(0)

const char *config_getstring(int notused)
{
    return NULL;
}

void syslog(int unused, const char *msg, ...)
{
    va_list argp;
    va_start(argp, msg);
    printf(msg, argp);
    printf("\n");
}

EXPORTED size_t strlcpy(char *dst, const char *src, size_t len)
{
    size_t n;

    if (len <= 0) {
        /* we can't do anything ! */
        return strlen(src);
    }

    /* assert(len >= 1); */
    for (n = 0; n < len-1; n++) {
	if ((dst[n] = src[n]) == '\0') break;
    }
    if (n >= len-1) {
	/* ran out of space */
	dst[n] = '\0';
	while(src[n]) n++;
    }
    return n;
}

EXPORTED size_t strlcat(char *dst, const char *src, size_t len)
{
    size_t i, j, o;
    
    o = strlen(dst);
    if (len < o + 1)
	return o + strlen(src);
    len -= o + 1;
    for (i = 0, j = o; i < len; i++, j++) {
	if ((dst[j] = src[i]) == '\0') break;
    }
    dst[j] = '\0';
    if (src[i] == '\0') {
	return j;
    } else {
	return j + strlen(src + i);
    }
}

static int add_arg(char *buf, int max_size, const char *arg, int *buflen)
{
    const char *myarg = (arg ? arg : "");
    int len = strlen(myarg) + 1;

    if (*buflen + len > max_size) return -1;

    strcat(buf+*buflen, myarg);
    *buflen += len;

    return 0;
}

EXPORTED void notify(const char *method,
	    const char *class, const char *priority,
	    const char *user, const char *mailbox,
	    int nopt, const char **options,
	    const char *message)
{
    const char *notify_sock;
    int soc = -1;
    struct sockaddr_un sun_data;
    char buf[NOTIFY_MAXSIZE] = "", noptstr[20];
    int buflen = 0;
    int i, r = 0;

    soc = socket(AF_UNIX, SOCK_DGRAM, 0);
    if (soc == -1) {
	syslog(LOG_ERR, "unable to create notify socket(): %m");
	goto out;
    }

    memset((char *)&sun_data, 0, sizeof(sun_data));
    sun_data.sun_family = AF_UNIX;
    notify_sock = config_getstring(IMAPOPT_NOTIFYSOCKET);
    if (notify_sock) {	
	strlcpy(sun_data.sun_path, notify_sock, sizeof(sun_data.sun_path));
    }
    else {
	strlcpy(sun_data.sun_path, config_dir, sizeof(sun_data.sun_path));
	strlcat(sun_data.sun_path,
		FNAME_NOTIFY_SOCK, sizeof(sun_data.sun_path));
    }
    //printf("SOCKET PATH IS %s\n", sun_data.sun_path);

    /*
     * build request of the form:
     *
     * method NUL class NUL priority NUL user NUL mailbox NUL
     *   nopt NUL N(option NUL) message NUL
     */

    r = add_arg(buf, sizeof(buf), method, &buflen);
    if (!r) r = add_arg(buf, sizeof(buf), class, &buflen);
    if (!r) r = add_arg(buf, sizeof(buf), priority, &buflen);
    if (!r) r = add_arg(buf, sizeof(buf), user, &buflen);
    if (!r) r = add_arg(buf, sizeof(buf), mailbox, &buflen);

    snprintf(noptstr, sizeof(noptstr), "%d", nopt);
    if (!r) r = add_arg(buf, sizeof(buf), noptstr, &buflen);

    for (i = 0; !r && i < nopt; i++) {
	r = add_arg(buf, sizeof(buf), options[i], &buflen);
    }

    if (!r) r = add_arg(buf, sizeof(buf), message, &buflen);

    if (r) {
        syslog(LOG_ERR, "notify datagram too large, %s, %s",
	       user, mailbox);
	goto out;
    }

    r = sendto(soc, buf, buflen, 0,
	       (struct sockaddr *)&sun_data, sizeof(sun_data));

    if (r < 0) {
	syslog(LOG_ERR, "unable to sendto() notify socket: %m");
	goto out;
    }
    if (r < buflen) {
	syslog(LOG_ERR, "short write to notify socket");
	goto out;
    }

out:
    xclose(soc);
    return;
}

const char *method = "donotusemethod";
const char *class = "donotuseclass";
const char *priority = "donotusepriority";
const char *user = "donotuseuser";
const char *mailbox = "donotusemailbox";
int nopt = 2;
const char *options[] = { "donotuseopt1", "donotuseopt2" };

void send_notification(char *msg, int msglen, const char *file)
{
    if (msg) {
        if (msg[msglen - 1] == '\n') {
            msg[msglen - 1] = '\0';
        } else {
            msg[msglen] = '\0';
        }
        //printf("%i \"%s\"\n", msglen, msg);
        printf("Sending a message from file %s\n", file);
        notify(method, class, priority, user, mailbox, nopt, options, msg);
    }
}

int main(int argc, const char *argv[])
{
    ssize_t msglinelen = 4192;
    char *msgline = malloc(msglinelen);
    ssize_t linelen = 4192;
    char *msg = NULL;
    ssize_t msglen = 0;
    int argn;
    FILE *fd = NULL;

    if (argc < 2) {
        printf("Please pass the path to one or more files containing notification messages.\n");
        return -1;
    }

    for (argn = 1; argn < argc; ++argn) {
        const char *file = argv[argn];
        fd = fopen(file, "r");
        if (!fd) {
            printf("Could not open %s\n", file);
            continue;
        }

        printf("\nReading file %s\n", file);
        //rewind(fd);
        while ((linelen = getline(&msgline, &msglinelen, fd)) > 0) {
            if (msgline[0] == '{' && msg) { // hack for multiline files. meh.
                send_notification(msg, msglen, file);
                free(msg);
                msg = NULL;
                msglen = 0;
            }

            msg = realloc(msg, linelen + msglen);
            memcpy(msg + msglen, msgline, linelen);
            msglen = linelen + msglen;
        }

        fclose(fd);
        fd = NULL;

        if (msg) {
            send_notification(msg, msglen, file);
            free(msg);
            msg = NULL;
            msglen = 0;
        }
    }

    free(msgline);
}

// gcc notify.c -o ~/bin/egara_json_notifier

