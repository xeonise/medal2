use futures_util::{SinkExt, StreamExt};
use log::*;
use std::net::SocketAddr;
use tokio::net::{TcpListener, TcpStream};
use base64::decode;
use luau_lifter::decompile_bytecode;
use tokio_tungstenite::{
    accept_async,
    tungstenite::{protocol::Message, Error, Result},
};

async fn accept_connection(peer: SocketAddr, stream: TcpStream) {
    if let Err(e) = handle_connection(peer, stream).await {
        match e {
            Error::ConnectionClosed | Error::Protocol(_) | Error::Utf8 => (),
            err => error!("Error processing connection: {}", err),
        }
    }
}

async fn decompiler(base64_b: String) -> Result<Message> {
    match decode(base64_b.as_bytes()) {
        Ok(bytecode) => {
            let decompiled = decompile_bytecode(&bytecode, 203);
            Ok(Message::text(format!("{}", decompiled)))
        }
        Err(e) => Ok(Message::text(format!("Failed to decode base64 bytecode: {}", e))),
    }
}

async fn handle_connection(peer: SocketAddr, stream: TcpStream) -> Result<()> {
    let mut ws_stream = accept_async(stream).await.expect("Failed to accept");

    info!("New WebSocket connection: {}", peer);

    while let Some(msg) = ws_stream.next().await {
        let msg = msg?;
        let sendmsg = decompiler(msg.to_string()).await?; // Adjusted this line
        if msg.is_text() || msg.is_binary() {
            ws_stream.send(sendmsg).await?;
        }
    }

    Ok(())
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let addr = "127.0.0.1:9002";
    let listener = TcpListener::bind(&addr).await.expect("Can't listen");
    info!("Listening on: {}", addr);

    while let Ok((stream, _)) = listener.accept().await {
        let peer = stream.peer_addr().expect("connected streams should have a peer address");
        info!("Peer address: {}", peer);

        tokio::spawn(accept_connection(peer, stream));
    }
}
