class Tokenizer inherits IO {
    initialStr: String;
    separator: String;
    strLen: Int;
    currentPosition : Int;

    init(inputStr: String, inputSep: String): Tokenizer {{
        initialStr <- inputStr;
        separator <- inputSep;
        strLen <- initialStr.length();
        currentPosition <- 0;
        self;
    }};

    isSeparator(): Bool {
        if initialStr.substr(currentPosition, 1) = separator
        then true
        else false
        fi
    };

    isEnd(): Bool {
        if currentPosition = strLen
        then true
        else false
        fi
    };

    notSeparatorNotEnd(): Bool {
        if not(isEnd())
        then
            if not(isSeparator())
            then true
            else false
            fi
        else false
        fi
    };

    hasMoreTokens(): Bool {
        if isEnd()
        then false
        else
            if isSeparator()
            then (new Tokenizer).init(initialStr.substr(currentPosition + 1, strLen - currentPosition - 1), separator).hasMoreTokens()
            else true
            fi
        fi
    };

    nextToken(): String {{
        while isSeparator() loop
            currentPosition <- currentPosition + 1
        pool;

        let currentToken: String
        in {
            while notSeparatorNotEnd() loop {
                currentToken <- currentToken.concat(initialStr.substr(currentPosition, 1));
                currentPosition <- currentPosition + 1;
            } pool;

            currentToken;
        };
    }};
};
