use futures_util::StreamExt;
use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Request, Response, Server};
use log::*;
use serde::Deserialize;
use std::convert::Infallible;
use std::net::SocketAddr;
use tokio::net::TcpListener;
use tokio::sync::mpsc;
use base64::decode;
use luau_lifter::decompile_bytecode;

async fn decompiler(base64_b: String) -> Result<String, String> {
    match decode(base64_b.as_bytes()) {
        Ok(bytecode) => {
            let decompiled = decompile_bytecode(&bytecode, 203);
            Ok(format!("{}", decompiled))
        }
        Err(e) => Err(format!("Failed to decode base64 bytecode: {}", e)),
    }
}

async fn handle_request(req: Request<Body>) -> Result<Response<Body>, Infallible> {
    let bytes = hyper::body::to_bytes(req.into_body()).await.unwrap();
    let base64_bytecode = String::from_utf8(bytes.to_vec()).unwrap();

    match decompiler(base64_bytecode).await {
        Ok(decompiled_code) => Ok(Response::new(Body::from(decompiled_code))),
        Err(e) => Ok(Response::new(Body::from(e))),
    }
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let addr = SocketAddr::from(([127, 0, 0, 1], 9002));

    let make_svc = make_service_fn(|_conn| {
        async {
            Ok::<_, Infallible>(service_fn(handle_request))
        }
    });

    let server = Server::bind(&addr).serve(make_svc);

    println!("Listening on http://{}", addr);

    if let Err(e) = server.await {
        eprintln!("Server error: {}", e);
    }
}
