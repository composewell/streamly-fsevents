/*
 * Code adapted from the Haskell "hfsevents" package.
 *
 * Copyright (c) 2012, Luite Stegeman
 *
 */

#include <config.h>

#if HAVE_DECL_KFSEVENTSTREAMCREATEFLAGFILEEVENTS

#include <CoreServices/CoreServices.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <FS/Event/Darwin.h>

/*
 * For reference documentaion see:
 * https://developer.apple.com/documentation/coreservices/file_system_events?language=objc
 * https://developer.apple.com/library/archive/documentation/Darwin/Conceptual/FSEvents_ProgGuide/UsingtheFSEventsFramework/UsingtheFSEventsFramework.html
 *
 * The FSEvents callbacks run on a dispatch queue. They write the events to
 * a pipe, the receiver reads them from the read end of the pipe.
 */

/******************************************************************************
 * Create Flags
 *****************************************************************************/

UInt32 FSEventStreamCreateFlagNoDefer () {
  return kFSEventStreamCreateFlagNoDefer;
}
UInt32 FSEventStreamCreateFlagWatchRoot () {
  return kFSEventStreamCreateFlagWatchRoot;
}
UInt32 FSEventStreamCreateFlagFileEvents () {
  return kFSEventStreamCreateFlagFileEvents;
}
UInt32 FSEventStreamCreateFlagIgnoreSelf () {
  return kFSEventStreamCreateFlagIgnoreSelf;
}

/*
 * The following flags are not available in older SDKs. When a flag is not
 * available we return 0, setting it is then a no-op and checking it always
 * returns false.
 */

UInt32 FSEventStreamCreateFlagFullHistory () {
#if HAVE_DECL_KFSEVENTSTREAMCREATEFLAGFULLHISTORY
  return kFSEventStreamCreateFlagFullHistory;
#else
  return 0;
#endif
}

/******************************************************************************
 * Event Flags
 *****************************************************************************/

UInt32 FSEventStreamEventFlagEventIdsWrapped () {
  return kFSEventStreamEventFlagEventIdsWrapped;
}
UInt32 FSEventStreamEventFlagMustScanSubDirs () {
  return kFSEventStreamEventFlagMustScanSubDirs;
}
UInt32 FSEventStreamEventFlagKernelDropped () {
  return kFSEventStreamEventFlagKernelDropped;
}
UInt32 FSEventStreamEventFlagUserDropped () {
  return kFSEventStreamEventFlagUserDropped;
}
UInt32 FSEventStreamEventFlagHistoryDone () {
  return kFSEventStreamEventFlagHistoryDone;
}
UInt32 FSEventStreamEventFlagRootChanged () {
  return kFSEventStreamEventFlagRootChanged;
}
UInt32 FSEventStreamEventFlagMount () {
  return kFSEventStreamEventFlagMount;
}
UInt32 FSEventStreamEventFlagUnmount () {
  return kFSEventStreamEventFlagUnmount;
}
UInt32 FSEventStreamEventFlagItemChangeOwner () {
  return kFSEventStreamEventFlagItemChangeOwner;
}
UInt32 FSEventStreamEventFlagItemInodeMetaMod () {
  return kFSEventStreamEventFlagItemInodeMetaMod;
}
UInt32 FSEventStreamEventFlagItemFinderInfoMod () {
  return kFSEventStreamEventFlagItemFinderInfoMod;
}
UInt32 FSEventStreamEventFlagItemXattrMod () {
  return kFSEventStreamEventFlagItemXattrMod;
}
UInt32 FSEventStreamEventFlagItemCreated () {
  return kFSEventStreamEventFlagItemCreated;
}
UInt32 FSEventStreamEventFlagItemRemoved () {
  return kFSEventStreamEventFlagItemRemoved;
}
UInt32 FSEventStreamEventFlagItemRenamed () {
  return kFSEventStreamEventFlagItemRenamed;
}
UInt32 FSEventStreamEventFlagItemModified () {
  return kFSEventStreamEventFlagItemModified;
}
UInt32 FSEventStreamEventFlagItemCloned () {
#if HAVE_DECL_KFSEVENTSTREAMEVENTFLAGITEMCLONED
  return kFSEventStreamEventFlagItemCloned;
#else
  return 0;
#endif
}
UInt32 FSEventStreamEventFlagItemIsDir () {
  return kFSEventStreamEventFlagItemIsDir;
}
UInt32 FSEventStreamEventFlagItemIsFile () {
  return kFSEventStreamEventFlagItemIsFile;
}
UInt32 FSEventStreamEventFlagItemIsSymlink () {
  return kFSEventStreamEventFlagItemIsSymlink;
}
/* IsHardlink and IsLastHardlink were introduced together in macOS 10.10 */
UInt32 FSEventStreamEventFlagItemIsHardlink () {
#if HAVE_DECL_KFSEVENTSTREAMEVENTFLAGITEMISHARDLINK
  return kFSEventStreamEventFlagItemIsHardlink;
#else
  return 0;
#endif
}
UInt32 FSEventStreamEventFlagItemIsLastHardlink () {
#if HAVE_DECL_KFSEVENTSTREAMEVENTFLAGITEMISHARDLINK
  return kFSEventStreamEventFlagItemIsLastHardlink;
#else
  return 0;
#endif
}

/******************************************************************************
 * Event watch
 *****************************************************************************/

/*
 * A watch is a dispatch queue and a pipe. The streams of a watch are
 * started and stopped by the Haskell code, see the Haskell module
 * Streamly.Internal.FS.Event.Darwin.Replace. The callbacks of all the
 * streams of a watch run on the serial queue and write the events to the
 * pipe. The receiver reads the events from the read end of the pipe.
 *
 * Each stream has a generation number, given by the caller of
 * startStream. A record in the pipe is five UInt64 values: the record
 * kind, the generation, the event id, the event flags, the path length,
 * followed by the path bytes. RECORD_EVENT is an event. RECORD_RETIRED is
 * written after the last event of a stopped stream, its other fields
 * except the generation are 0.
 */

#define RECORD_EVENT 0
#define RECORD_RETIRED 1

struct watch
{
    dispatch_queue_t queue;
    int writefd;
    double latency;
};

struct streamCtx
{
    struct watch *w;
    FSEventStreamRef stream;
    UInt64 gen;
};

/* Write all the bytes, give up on an error, e.g. EPIPE */
static void writeAll(int fd, const void *buf, size_t len)
{
    const char *p = buf;
    while (len > 0) {
        ssize_t n = write(fd, p, len);
        if (n < 0) {
            if (errno == EINTR) {
                continue;
            }
            return;
        }
        p += n;
        len -= (size_t)n;
    }
}

static void writeRecord
    ( int fd
    , UInt64 kind
    , UInt64 gen
    , UInt64 eventId
    , UInt64 eventFlags
    , const char *path
    )
{
    UInt64 buf[5];
    /* XXX Is the path string in UTF-8? */
    size_t len = path ? strlen(path) : 0;

    buf[0] = kind;
    buf[1] = gen;
    buf[2] = eventId;
    buf[3] = eventFlags;
    buf[4] = (UInt64)len;
    writeAll(fd, buf, sizeof buf);
    writeAll(fd, path, len);
}

static void watchCallback
    ( ConstFSEventStreamRef streamRef
    , void *clientCallBackInfo
    , size_t n
    , void *eventPaths
    , const FSEventStreamEventFlags eventFlags[]
    , const FSEventStreamEventId eventIds[]
    )
{
    int i;
    struct streamCtx *ctx = clientCallBackInfo;
    char **paths = eventPaths;

    for (i = 0; i < n; i++) {
        writeRecord
            ( ctx->w->writefd, RECORD_EVENT, ctx->gen, eventIds[i]
            , eventFlags[i], paths[i]);
    }
}

#define MAX_WATCH_PATHS 4096

static void free_cffolders(CFStringRef *cffolders, int n) {
    int i;
    for (i = 0; i < n; i++) {
        CFRelease (cffolders[i]);
    }
    free(cffolders);
}

static CFArrayRef makePathArray(struct pathName *folders, int n)
{
    int i;
    CFStringRef *cffolders = malloc(n * sizeof(CFStringRef));
    if (!cffolders) {
      return NULL;
    }

    for(i = 0; i < n; i++) {
      cffolders[i] = CFStringCreateWithBytes
          ( NULL
          , folders[i].pathBytes
          , folders[i].pathLen
          , kCFStringEncodingUTF8
          , false
          );
      if (!cffolders[i]) {
          free_cffolders(cffolders, i);
          return NULL;
      }
    }
    /* The array retains the strings */
    CFArrayRef paths =
        CFArrayCreate
            (NULL, (const void **)cffolders, n, &kCFTypeArrayCallBacks);
    free_cffolders(cffolders, n);
    return paths;
}

UInt64 currentEventId(void)
{
    return FSEventsGetCurrentEventId();
}

UInt64 eventIdSinceNow(void)
{
    return kFSEventStreamEventIdSinceNow;
}

/*
 * Create and start a stream on the n paths in folders, on the queue of the
 * watch, with the generation number gen. Returns NULL on failure.
 */
void *startStream
    ( struct watch *w
    , struct pathName* folders
    , int n
    , UInt64 gen
    , UInt64 since
    , UInt32 createFlags
    )
{
    if (n <= 0 || n > MAX_WATCH_PATHS) {
      return NULL;
    }

    struct streamCtx *ctx = malloc(sizeof(struct streamCtx));
    if (!ctx) {
      return NULL;
    }
    ctx->w = w;
    ctx->gen = gen;

    CFArrayRef paths = makePathArray(folders, n);
    if (!paths) {
      goto cleanup_ctx;
    }

    FSEventStreamContext fsctx;
    fsctx.version = 0;
    fsctx.info = (void*)ctx;
    fsctx.retain = NULL;
    fsctx.release = NULL;
    fsctx.copyDescription = NULL;

    ctx->stream = FSEventStreamCreate
        ( NULL, &watchCallback, &fsctx, paths, since, w->latency
        , createFlags);
    CFRelease(paths);

    if (ctx->stream == NULL) {
      goto cleanup_ctx;
    }

    FSEventStreamSetDispatchQueue(ctx->stream, w->queue);
    /*
     * Start fails e.g. for the streams beyond the 512th in a process.
     */
    if (!FSEventStreamStart(ctx->stream)) {
      /* Stop is not allowed on a stream that is not started */
      FSEventStreamInvalidate(ctx->stream);
      FSEventStreamRelease(ctx->stream);
      goto cleanup_ctx;
    }
    return ctx;

cleanup_ctx:
    free(ctx);
    return NULL;
}

/* Runs on the queue, after the last callback of the stream */
static void retireStream(void *p)
{
    struct streamCtx *ctx = p;

    FSEventStreamRelease(ctx->stream);
    writeRecord(ctx->w->writefd, RECORD_RETIRED, ctx->gen, 0, 0, NULL);
    free(ctx);
}

/*
 * Stop a stream started by startStream. The stream is released, and its
 * retired record is written, on the queue after its last callback. Does
 * not wait for the queue. Must not be called after destroyWatch.
 */
void stopStream(struct streamCtx *ctx)
{
    /*
     * Stop and invalidate do not wait for a callback running on the queue,
     * it may run after they return. The callbacks already queued run
     * before retireStream.
     */
    FSEventStreamStop(ctx->stream);
    FSEventStreamInvalidate(ctx->stream);
    dispatch_async_f(ctx->w->queue, ctx, retireStream);
}

/*
 * Create a watch without any stream. startStream starts the streams.
 */
int createWatch
    ( double latency
    , int* fd
    , void** wp
    )
{
    int pfds[2];
    if (pipe (pfds)) {
      return -1;
    }

    /* Do not leak the pipe to child processes */
    if (fcntl(pfds[0], F_SETFD, FD_CLOEXEC)
            || fcntl(pfds[1], F_SETFD, FD_CLOEXEC)) {
      goto cleanup_pipe;
    }

    /*
     * The receiver waits for the read end to become readable via the RTS.
     */
    if (fcntl(pfds[0], F_SETFL, O_NONBLOCK)) {
      goto cleanup_pipe;
    }

    /*
     * The read end may be closed before the streams are stopped, the
     * callback may write to the pipe after that. Return EPIPE instead of
     * raising SIGPIPE, which terminates the process unless it is ignored.
     */
    if (fcntl(pfds[1], F_SETNOSIGPIPE, 1)) {
      goto cleanup_pipe;
    }

    struct watch *w = malloc(sizeof(struct watch));
    if (!w) {
      goto cleanup_pipe;
    }

    w->queue = dispatch_queue_create("com.composewell.streamly", NULL);
    if (!w->queue) {
      free(w);
      goto cleanup_pipe;
    }

    w->writefd = pfds[1];
    w->latency = latency;
    *fd = pfds[0];
    *wp = w;
    return 0;

cleanup_pipe:
    close(pfds[0]);
    close(pfds[1]);
    return -1;
}

/******************************************************************************
 * Stop a watch event loop
 *****************************************************************************/

static void noop(void *ctx) {
    (void)ctx;
}

/*
 * Destroy a watch. All its streams must have been stopped by stopStream.
 * Waits for the callbacks and the retireStream of the streams to finish.
 */
void destroyWatch(struct watch* w) {
    /*
     * A write to a full pipe stays blocked as long as the read end is open
     * in any process. The receiver may not be reading the pipe. Closing our
     * read fd does not close the read end if a child process holds a copy
     * of it. The read fd has FD_CLOEXEC, but a child can still hold a copy
     * if it forked before FD_CLOEXEC was set on the fd (pipe and fcntl are
     * separate calls), or if it forked without exec.
     *
     * Replacing the write fd with /dev/null wakes up the blocked write.
     * The remaining writes of the callback go to /dev/null and complete,
     * and the callback returns. The read end then reports end of file, if
     * no child process holds a copy of the write end.
     *
     * We use dup2 instead of close. The callback writes to the fd number
     * in w->writefd. Close would free that number, and a file or socket
     * opened by another thread could get the same number. The remaining
     * writes of the callback would then put event data into that file or
     * socket. dup2 keeps the number allocated to /dev/null until the close
     * below, which runs after the callback has finished.
     */
    int devnull = open("/dev/null", O_WRONLY | O_CLOEXEC);
    if (devnull >= 0) {
        dup2(devnull, w->writefd);
        close(devnull);
    }

    /*
     * Wait for the callback running on the serial queue, if any, and for
     * the retireStream of the stopped streams, which release them.
     */
    dispatch_sync_f(w->queue, NULL, noop);
    dispatch_release(w->queue);
    close(w->writefd);
    free(w);
}

/******************************************************************************
 * File identity
 *****************************************************************************/

/*
 * The device, inode and generation numbers of the file at the path,
 * following the symbolic links. st_gen is 0 unless the process runs as
 * root. Returns 0 on success, -1 with errno set on failure.
 */
int fileId(const char *path, UInt64 *dev, UInt64 *ino, UInt64 *gen)
{
    struct stat st;

    if (stat(path, &st) != 0) {
        return -1;
    }
    *dev = (UInt64)st.st_dev;
    *ino = (UInt64)st.st_ino;
    *gen = (UInt64)st.st_gen;
    return 0;
}

#endif
