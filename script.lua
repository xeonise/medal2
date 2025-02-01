function medaldecom(script)
    httpresponse = request({
        Url = "http://127.0.0.1:9002",
        Body = base64.encode(getscriptbytecode(script)),  -- The base64 encoded bytecode
        Method = "POST",
        Headers = {
            ["Content-Type"] = "text/plain"
        },
    })
    return httpresponse.Body
end
getgenv().decompile = medaldecom
