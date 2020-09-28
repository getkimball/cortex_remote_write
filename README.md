# cortex_remote_write
Erlang library to forward Prometheus metrics to a Cortex/Prometheus instance

Tested writing to cortex, might also work with Prometheus directly.

Collects metrics from the `default` registry of [Prometheus lib](https://github.com/deadtrickster/prometheus.erl).

## Config

Example:

```
[{cortex_remote_write, [
    {url, "URL"},
    {username, "USERNAME"},
    {password, "PASSWORD"},
    {default_labels, [
      {"label_name", "label_value"}
    ]}
]}].
```

If `url` is not specified, the supervisor will not start the writer process.

`username` and `password` are required if `url` is present.

`default_labels` defaults to `[]`.
