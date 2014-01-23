-record(history, {
        time :: tuple(),
        account :: string(),
        data :: string()
    }).

-record(account, {
        name :: string() | undefined,
        avatar :: string() | undefined,
        token :: string() | undefined
    }).
