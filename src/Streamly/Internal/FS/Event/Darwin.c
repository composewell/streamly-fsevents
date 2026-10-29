/*
 * Code adapted from the Haskell "hfsevents" package.
 *
 * Copyright (c) 2012, Luite Stegeman
 *
 */

#include <config.h>

#if HAVE_DECL_KFSEVENTSTREAMCREATEFLAGFILEEVENTS

#include <CoreServices/CoreServices.h>
#include <fcntl.h>
#include <pthread.h>
#include <unistd.h>
#include <FS/Event/Darwin.h>

/*
 * For reference documentaion see:
 * https://developer.apple.com/documentation/coreservices/file_system_events?language=objc
 * https://developer.apple.com/library/archive/documentation/Darwin/Conceptual/FSEvents_ProgGuide/UsingtheFSEventsFramework/UsingtheFSEventsFramework.html
 *
 * An OS thread is started which runs the event loop. A pipe is created
 * and the events are sent to the pipes. The receiver can read the pipe
 * output end to get the events.
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

/* Write an event to the pipe input fd */
static void writeEvent(int fd, UInt64 eventId, UInt64 eventFlags, char* path)
{
    UInt64 buf[3];
    /* XXX Is the path string in UTF-8? */
    size_t len = strlen(path);

    buf[0] = eventId;
    buf[1] = eventFlags;
    buf[2] = (UInt64)len;
    write(fd, buf, 3 * sizeof(UInt64));
    write(fd, path, len);
}

struct watch
{
    FSEventStreamRef eventStream;
    dispatch_queue_t queue;
    int writefd;
};

/* Just writes the event to the pipe input fd */
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
    struct watch *w = clientCallBackInfo;
    char **paths = eventPaths;

    for (i = 0; i < n; i++) {
        writeEvent(w->writefd, eventIds[i], eventFlags[i], paths[i]);
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

int createWatch
    ( struct pathName* folders
    , int n  /* number of entries in folders */
    , UInt32 createFlags
    , UInt64 since
    , double latency
    , int* fd
    , void** wp
    )
{
    if (n > MAX_WATCH_PATHS) {
      return -1;
    }

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
     * The read end is closed before the stream is stopped, the callback may
     * write to the pipe after that. Return EPIPE instead of raising SIGPIPE,
     * which terminates the process unless it is ignored.
     */
    if (fcntl(pfds[1], F_SETNOSIGPIPE, 1)) {
      goto cleanup_pipe;
    }

    /*
     * XXX We can possibly use since == 0 to get all events since
     * beginning of time
     */
    if (!since) {
      since = kFSEventStreamEventIdSinceNow;
    }

    struct watch *w = malloc(sizeof(struct watch));
    if (!w) {
      goto cleanup_pipe;
    }

    /* Setup paths array */
    CFStringRef *cffolders = malloc(n * sizeof(CFStringRef));
    if (!cffolders) {
      goto cleanup_watch;
    }

    /* Create event stream using paths and context*/
    int i;
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
          goto cleanup_watch;
      }
    }
    CFArrayRef paths = CFArrayCreate(NULL, (const void **)cffolders, n, NULL);
    if (!paths) {
      free_cffolders(cffolders, n);
      goto cleanup_watch;
    }

    FSEventStreamContext ctx;
    ctx.version = 0;
    ctx.info = (void*)w;
    ctx.retain = NULL;
    ctx.release = NULL;
    ctx.copyDescription = NULL;

    w->eventStream = FSEventStreamCreate
        (NULL, &watchCallback, &ctx, paths, since, latency, createFlags);
    free_cffolders(cffolders, n);
    CFRelease(paths);

    if(w->eventStream == NULL) {
      goto cleanup_watch;
    }

    w->queue = dispatch_queue_create("com.composewell.streamly", NULL);
    if (!w->queue) {
      goto cleanup_es;
    }

    w->writefd = pfds[1];
    *fd = pfds[0];
    *wp = w;
    FSEventStreamSetDispatchQueue(w->eventStream, w->queue);
    /*
     * Start fails e.g. for the streams beyond the 512th in a process. The
     * stream would never call the callback, the watch would report no
     * events.
     */
    if (!FSEventStreamStart(w->eventStream)) {
      /* Stop is not allowed on a stream that is not started */
      FSEventStreamInvalidate(w->eventStream);
      dispatch_release(w->queue);
      goto cleanup_es;
    }
    return 0;

cleanup_es:
    FSEventStreamRelease(w->eventStream);
cleanup_watch:
    free(w);
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
 * The read end of the pipe must be closed before calling this, so that a
 * callback blocked writing to a full pipe fails with EPIPE.
 */
void destroyWatch(struct watch* w) {
    /*
     * Stop and invalidate do not wait for a callback running on the queue,
     * it may run after they return.
     */
    FSEventStreamStop(w->eventStream);
    FSEventStreamInvalidate(w->eventStream);

    /*
     * A callback may still be blocked writing to a full pipe when the read
     * end is open in another process, e.g. a child that inherited it.
     * Replacing the write end with /dev/null wakes up the write. Unlike
     * close, dup2 keeps the fd number allocated, the callback cannot write
     * to a file opened later with the same number.
     */
    int devnull = open("/dev/null", O_WRONLY | O_CLOEXEC);
    if (devnull >= 0) {
        dup2(devnull, w->writefd);
        close(devnull);
    }

    /* Wait for the callback running on the serial queue, if any */
    dispatch_sync_f(w->queue, NULL, noop);
    dispatch_release(w->queue);
    FSEventStreamRelease(w->eventStream);
    close(w->writefd);
    free(w);
}
#endif
