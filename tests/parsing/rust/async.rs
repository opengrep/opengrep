/*
 * ============================================
 * Async function
 * ============================================
 */

async fn abc() {}

async fn main() {
    let x = futures.await?;
}

/*
 * ============================================
 * Await expression
 * ============================================
 */

async fn main() {
  futures.await;
  futures.await?;
  futures.await?.await?;
  futures.await?.function().await?;
}

/*
 * ============================================
 * Async Block
 * ============================================
 */

async fn main() {
  async {}
  async { let x = 10; }
  async move {}
}

/*
 * ============================================
 * Async closure
 * ============================================
 */

async fn main() {
  let a = async || { foo() };
  let b = async move || { foo() };
  let c = async |x: u64| { foo(x) };
  let d = async move |x: u64| { foo(x) };
  let e = async |x| x.await;
}
