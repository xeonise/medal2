use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Request, Response, Server};
use base64::decode;
use luau_lifter::decompile_bytecode;
use std::convert::Infallible;

#[tokio::main]
async fn main() {
    let make_svc = make_service_fn(|_conn| {
        async {
            Ok::<_, Infallible>(service_fn(decompiler))
        }
    });

    let addr = ([127, 0, 0, 1], 8374).into();

    let server = Server::bind(&addr).serve(make_svc);

    println!("Listening on http://{}", addr);

    if let Err(e) = server.await {
        eprintln!("Server error: {}", e);
    }
}

async fn decompiler(req: Request<Body>) -> Result<Response<Body>, Infallible> {
    let headers = req.headers();

    if let Some(base64_bytecode) = headers.get("decompile") {
        match decode(base64_bytecode.as_bytes()) {
            Ok(bytecode) => {
                let decompiled = decompile_bytecode(&bytecode, 203);
                Ok(Response::new(Body::from(format!("{}", decompiled))))
            }
            Err(e) => {
                Ok(Response::new(Body::from(format!("Failed to decode base64 bytecode: {}", e))))
            }
        }
    } else {
        Ok(Response::new(Body::from("Missing 'decompile' header")))
    }
}
