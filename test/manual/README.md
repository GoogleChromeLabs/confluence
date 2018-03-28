# UI tests with manual verification

## Setup

1. The `dev` webpack build produces required JavaScript bundles:

    1. One-off

    ```bash
    cd /path/to/confluence
    webpack --config=config/webpack.dev.js
    ```

    OR

    1. Live reload

    ```bash
    cd /path/to/confluence
    webpack --watch --progress --config=config/webpack.dev.js
    ```

## Testing

Visit HTML files in `file:///path/to/confluence/test/manual/` and verify
expectations.
