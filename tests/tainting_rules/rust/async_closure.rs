async fn tainted_in_async_closure() {
    let data = source();
    let run = async || {
        // ruleid: taint
        sink(data);
    };
    run().await;
}

async fn clean_in_async_closure() {
    let data = String::new();
    let run = async move || {
        // ok: taint
        sink(data);
    };
    run().await;
}
