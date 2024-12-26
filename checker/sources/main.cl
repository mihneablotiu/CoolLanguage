class Main inherits IO {
    loader: Loader <- new Loader;

    checkHelp(parameters: List): Bool {
        if not(parameters.length() = 0)
        then false
        else { out_string("Comenzile posibile sunt:\nhelp\nload\nprint\nmerge\nfilterBy\nsortBy\n"); true; }
        fi
    };

    checkPrint(parameters: List): Bool {{
        if 1 < parameters.length()
        then false
        else
            let allLists: List <- loader.getGeneralList()
            in (
                if parameters.length() = 0
                then { out_string(allLists.toString()); true; }
                else
                    case parameters.head() of
                        x: String => (
                            let listNumber: Int <- (new A2I).a2i(x)
                            in (
                                if allLists.length() < listNumber
                                then false
                                else { out_string("[ ".concat(allLists.getListFromPosition(listNumber - 1).toString()).concat(" ]\n")); true; }
                                fi
                            )
                        );
                    esac
                fi
            )
        fi;
    }};

    checkMerge(parameters: List): Bool {{
        if not(parameters.length() = 2)
        then false
        else
            let firstParam: Object <- parameters.head(),
                secondParam: Object <- parameters.tail().head()
            in (
                case firstParam of
                    x: String => (
                        case secondParam of
                            y: String => (
                                let firstIndex: Int <- (new A2I).a2i(x),
                                    secondIndex: Int <- (new A2I).a2i(y),
                                    allLists: List <- loader.getGeneralList()
                                in (
                                    if allLists.length() < firstIndex
                                    then false
                                    else
                                        if allLists.length() < secondIndex
                                        then false
                                        else { loader.mergeLists(firstIndex - 1, secondIndex - 1); true; }
                                        fi
                                    fi
                                )
                            );
                        esac
                    );
                esac
            )
        fi;
    }};

    checkFilter(parameters: List): Bool {{
        if not(parameters.length() = 2)
        then false
        else
            let firstParam: Object <- parameters.head(),
                secondParam: Object <- parameters.tail().head()
            in (
                case firstParam of
                    x: String => (
                        case secondParam of
                            y: String => (
                                let listIndex: Int <- (new A2I).a2i(x),
                                    allLists: List <- loader.getGeneralList()
                                in (
                                    if allLists.length() < listIndex
                                    then false
                                    else
                                        if y = "ProductFilter"
                                        then { loader.filterList(listIndex - 1, (new ProductFilter)); true; }
                                        else
                                            if y = "RankFilter"
                                            then { loader.filterList(listIndex - 1, (new RankFilter)); true; }
                                            else
                                                if y = "SamePriceFilter"
                                                then { loader.filterList(listIndex - 1, (new SamePriceFilter)); true; }
                                                else false
                                                fi
                                            fi
                                        fi
                                    fi
                                )
                            );
                        esac
                    );
                esac
            )
        fi;
    }};

    checkSort(parameters: List): Bool {{
        if not(parameters.length() = 3)
        then false
        else
            let firstParam: Object <- parameters.head(),
                secondParam: Object <- parameters.tail().head(),
                thirdParam: Object <- parameters.tail().tail().head()
            in (
                case firstParam of
                    x: String => (
                        case secondParam of
                            y: String => (
                                case thirdParam of
                                    z: String => (
                                        let listIndex: Int <- (new A2I).a2i(x),
                                        allLists: List <- loader.getGeneralList()
                                        in (
                                            if allLists.length() < listIndex
                                            then false
                                            else
                                                if z = "ascendent"
                                                then
                                                    if y = "PriceComparator"
                                                    then { loader.sortList(listIndex - 1, (new PriceComparator), z); true; }
                                                    else
                                                        if y = "RankComparator"
                                                        then { loader.sortList(listIndex - 1, (new RankComparator), z); true; }
                                                        else
                                                            if y = "AlphabeticComparator"
                                                            then { loader.sortList(listIndex - 1, (new AlphabeticComparator), z); true; }
                                                            else false
                                                            fi
                                                        fi
                                                    fi
                                                else
                                                    if z = "descendent"
                                                    then
                                                        if y = "PriceComparator"
                                                        then { loader.sortList(listIndex - 1, (new PriceComparator), z); true; }
                                                        else
                                                            if y = "RankComparator"
                                                            then { loader.sortList(listIndex - 1, (new RankComparator), z); true; }
                                                            else
                                                                if y = "AlphabeticComparator"
                                                                then { loader.sortList(listIndex - 1, (new AlphabeticComparator), z); true; }
                                                                else false
                                                                fi
                                                            fi
                                                        fi
                                                    else false
                                                    fi
                                                fi
                                            fi
                                        )
                                    );
                                esac
                            );
                        esac
                    );
                esac
            )
        fi;
    }};

    commandLineValidation(currentCommand: List): Bool {
        let currentLength: Int <- currentCommand.length()
        in (
            if currentLength < 1
            then false
            else
                case currentCommand.head() of
                    x: String => (
                        let parameters: List <- currentCommand.tail()
                        in (
                            if x = "help"
                            then checkHelp(parameters)
                            else
                                if x = "print"
                                then checkPrint(parameters)
                                else
                                    if x = "load"
                                    then loader.loadInput()
                                    else
                                        if x = "merge"
                                        then checkMerge(parameters)
                                        else 
                                            if x = "filterBy"
                                            then checkFilter(parameters)
                                            else
                                               if x = "sortBy"
                                               then checkSort(parameters)
                                               else false
                                               fi
                                            fi
                                        fi
                                    fi
                                fi
                            fi
                        )
                    );
                esac
            fi
        )
    };
    
    main(): Object {
        if not(loader.loadInput())
        then abort()
        else 
            let somestr: String <- in_string(),
                looping: Bool <- not(somestr = "exit")
            in (
                while looping loop {
                    let currentCommandList: List <- new List,
                        tokenizer: Tokenizer <- (new Tokenizer).init(somestr, " ")
                    in {
                        while(tokenizer.hasMoreTokens()) loop
                            let newToken: String <- tokenizer.nextToken()
                            in currentCommandList <- currentCommandList.addToEnd(newToken)
                        pool;

                        if not(commandLineValidation(currentCommandList))
                        then looping <- false
                        else {
                            somestr <- in_string();
                            if somestr = "exit"
                            then looping <- false
                            else looping <- true
                            fi;
                        }
                        fi;
                    };
                } pool
            )
        fi
    };
};