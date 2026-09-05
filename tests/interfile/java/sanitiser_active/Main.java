class Main {
    void run() {
        Senders.sendClean(source());
        Senders.sendDirty(source());
    }
}
