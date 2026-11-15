#include <config.h>

struct watch;
struct streamCtx;

struct pathName {
    const UInt8 *pathBytes;
    size_t pathLen;
};

int createWatch
    ( double latency
    , int* fd
    , void** wp
    );

void *startStream
    ( struct watch* w
    , struct pathName* folders
    , int n  /* number of entries in "folders" */
    , UInt64 gen
    , UInt64 since
    , UInt32 createFlags
    );

void stopStream(struct streamCtx* ctx);

void destroyWatch(struct watch* w);

UInt64 currentEventId(void);
UInt64 eventIdSinceNow(void);

/******************************************************************************
 * Create Flags
 *****************************************************************************/

UInt32 FSEventStreamCreateFlagNoDefer ();
UInt32 FSEventStreamCreateFlagWatchRoot ();
UInt32 FSEventStreamCreateFlagFileEvents ();
UInt32 FSEventStreamCreateFlagIgnoreSelf ();
UInt32 FSEventStreamCreateFlagFullHistory ();

/******************************************************************************
 * Event Flags
 *****************************************************************************/

UInt32 FSEventStreamEventFlagEventIdsWrapped ();
UInt32 FSEventStreamEventFlagMustScanSubDirs ();
UInt32 FSEventStreamEventFlagKernelDropped ();
UInt32 FSEventStreamEventFlagUserDropped ();
UInt32 FSEventStreamEventFlagHistoryDone ();
UInt32 FSEventStreamEventFlagRootChanged ();
UInt32 FSEventStreamEventFlagMount ();
UInt32 FSEventStreamEventFlagUnmount ();
UInt32 FSEventStreamEventFlagItemChangeOwner ();
UInt32 FSEventStreamEventFlagItemInodeMetaMod ();
UInt32 FSEventStreamEventFlagItemFinderInfoMod ();
UInt32 FSEventStreamEventFlagItemXattrMod ();
UInt32 FSEventStreamEventFlagItemCreated ();
UInt32 FSEventStreamEventFlagItemRemoved ();
UInt32 FSEventStreamEventFlagItemRenamed ();
UInt32 FSEventStreamEventFlagItemModified ();
UInt32 FSEventStreamEventFlagItemCloned ();
UInt32 FSEventStreamEventFlagItemIsDir ();
UInt32 FSEventStreamEventFlagItemIsFile ();
UInt32 FSEventStreamEventFlagItemIsSymlink ();
UInt32 FSEventStreamEventFlagItemIsHardlink ();
UInt32 FSEventStreamEventFlagItemIsLastHardlink ();
