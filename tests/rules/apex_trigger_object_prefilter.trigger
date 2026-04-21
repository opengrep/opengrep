// ruleid: apex-trigger-any-events
trigger BeforeInsertTrigger on Account (before insert) {
    System.debug('ok');
}
