package app;

public class Main {
    void run() {
        new Child().handle(source());
    }

    void runOwn() {
        new OwnMember().handle(source());
    }
}
