-record(account, {
        name :: string() | undefined,
        avatar :: string() | undefined,
        token :: string() | undefined
    }).

-record(history, {
        time :: tuple(),
        account :: string() | #account{},
        data :: binary()
    }).
