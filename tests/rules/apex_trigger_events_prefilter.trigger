// ruleid: apex-trigger-events
trigger AccountTrigger on Account (before insert, after update) {
    for (Account a : Trigger.new) {
        a.Description = 'hi';
    }
}
