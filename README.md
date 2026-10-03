# smcdel-mcp

A [Model Context Protocol](https://modelcontextprotocol.io) server that exposes
the [SMCDEL](https://github.com/jrclogic/SMCDEL) symbolic model checker for
Dynamic Epistemic Logic as tools, in the same way the SMCDEL web interface does.

The server offers one tool per SMCDEL query:

| Tool           | SMCDEL command | Arguments                              | Result                                   |
|----------------|----------------|----------------------------------------|------------------------------------------|
| `smcdel_true`  | `TRUE?`        | `structure`, `state`, `formula`        | whether the formula holds at the state   |
| `smcdel_valid` | `VALID?`       | `structure`, `formula`                 | whether the formula holds at all states  |
| `smcdel_where` | `WHERE?`       | `structure`, `formula`, `maxStates`    | the states where the formula holds       |

`structure` is the knowledge structure in SMCDEL syntax, i.e. the `VARS`, `LAW`
and `OBS` block of an `.smcdel.txt` file. `formula` uses the same syntax as the
queries in such a file. `state` is the list of true atomic propositions.
`UPDATE!` is not supported; every call is self-contained and the server keeps
no sessions.

The `initialize` response carries a short primer on the SMCDEL syntax in its
`instructions` field, and every tool has a JSON input and output schema.
Syntax errors, failed sanity checks and SMCDEL runtime errors are reported as
tool results with `isError: true` so that a model can correct its input.

## Building and running

```sh
stack build
stack exec smcdel-mcp            # Streamable HTTP
stack exec -- smcdel-mcp --stdio # stdio
```

Use `stack install` to copy the executable to `~/.local/bin`, which is the
easiest way to let an MCP client start it.

Over HTTP the server listens on `http://127.0.0.1:3000/mcp` by default. It is
configured through environment variables:

| Variable         | Default     | Meaning                                  |
|------------------|-------------|------------------------------------------|
| `PORT`           | `3000`      | TCP port (HTTP only)                     |
| `HOST`           | `127.0.0.1` | interface to bind to (HTTP only)         |
| `SMCDEL_TIMEOUT` | `30`        | per-query timeout in seconds             |

Run the tests with `stack test`.

## Transports

### stdio

With `--stdio` the client starts the server as a subprocess and talks to it
over stdin and stdout, so there is no server to start and no port to open.
Each line on stdin is one JSON-RPC 2.0 message; each request is answered with
one line of JSON on stdout, notifications get no reply. The server exits when
stdin is closed. Nothing but JSON-RPC messages is written to stdout.

### Streamable HTTP

Without arguments the server implements the MCP Streamable HTTP transport,
stateless and without server-sent events: `POST /mcp` takes one JSON-RPC 2.0
message. Requests are answered with a single JSON-RPC response as
`application/json`, notifications with `202 Accepted`. `GET` and `DELETE` on
`/mcp` return `405`. Requests with a non-local `Origin` header are rejected
with `403`.

## Using it with opencode

Add this to `opencode.json` in your project or to
`~/.config/opencode/opencode.json`; opencode then starts the server itself:

```json
{
  "$schema": "https://opencode.ai/config.json",
  "mcp": {
    "smcdel": {
      "type": "local",
      "command": ["smcdel-mcp", "--stdio"],
      "environment": { "SMCDEL_TIMEOUT": "30" },
      "enabled": true
    }
  }
}
```

If `~/.local/bin` is not on the `PATH` that opencode sees, give the full path
to the executable instead.

## Using it with Claude Code

Over stdio, started by Claude Code:

```sh
claude mcp add smcdel -- smcdel-mcp --stdio
```

Or over HTTP, with the server already running:

```sh
claude mcp add --transport http smcdel http://127.0.0.1:3000/mcp
```

## Example with curl

```sh
curl -s -X POST http://127.0.0.1:3000/mcp \
  -H 'Content-Type: application/json' \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{
        "name":"smcdel_where",
        "arguments":{
          "structure":"VARS 1,2,3\nLAW Top\nOBS alice: 2,3\n bob: 1,3\n carol: 1,2",
          "formula":"< ! (1|2|3) > ( (alice knows whether 1) | (bob knows whether 2) | (carol knows whether 3) )"
        }}}'
```

```json
{"id":1,"jsonrpc":"2.0","result":{
  "content":[{"type":"text","text":"At which states is < ! (1|2|3) > ( ... ) true?\nNumber of states: 3\n{1}\n{2}\n{3}\n"}],
  "isError":false,
  "structuredContent":{"count":3,"states":[[1],[2],[3]],"truncated":false,
    "formula":"< ! (1|2|3) > ( ... )",
    "parsed":"~[! ((1 | 2) | 3)] ~((Kw alice 1 | Kw bob 2) | Kw carol 3)"}}}
```

## Code layout

- `src/SMCDEL/MCP/JsonRpc.hs`: JSON-RPC 2.0 messages and error codes.
- `src/SMCDEL/MCP/Protocol.hs`: the transport-independent MCP dispatcher
  (`initialize`, `ping`, `tools/list`, `tools/call`).
- `src/SMCDEL/MCP/Tools.hs`: the three tools, their schemas and the syntax primer.
- `src/SMCDEL/MCP/Check.hs`: glue to the SMCDEL library (lexing, parsing,
  sanity checks, BDD based model checking, timeouts).
- `src/SMCDEL/MCP/Server.hs`: the scotty application (HTTP transport).
- `src/SMCDEL/MCP/Stdio.hs`: the stdio transport.
- `app/Main.hs`: the executable; `--stdio` selects the transport.
- `test/`: hspec tests for the dispatcher and both transports.
