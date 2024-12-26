class Loader inherits IO {
    generalList: List <- new List;
    currentList: List;

    loadInput(): Bool {{
        currentList <- new List;

        let somestr: String <- in_string(),
            loadResult: Bool <- true
        in {
            while not(somestr = "END") loop {
                let currentTokensList: List <- new List,
                    tokenizer: Tokenizer <- (new Tokenizer).init(somestr, " ")
                in {
                    while(tokenizer.hasMoreTokens()) loop
                        let newToken: String <- tokenizer.nextToken()
                        in currentTokensList <- currentTokensList.addToEnd(newToken)
                    pool;

                    if not(inputLineValidation(currentTokensList))
                    then { abort(); loadResult <- false; }
                    else somestr <- in_string()
                    fi;
                };
            } pool;

            generalList <- generalList.addToEnd(currentList);
            loadResult;
        };
    }};

    mergeLists(firstIndex: Int, secondIndex: Int): Bool {
        let firstList: List <- generalList.getListFromPosition(firstIndex),
            secondList: List <- generalList.getListFromPosition(secondIndex),
            concatenateLists: List <- firstList.concatenateLists(secondList)
        in {
            if firstIndex < secondIndex
            then {
                generalList <- generalList.removeListFromPosition(secondIndex);
                generalList <- generalList.removeListFromPosition(firstIndex);
            } else {
                generalList <- generalList.removeListFromPosition(firstIndex);
                generalList <- generalList.removeListFromPosition(secondIndex);
            }
            fi;
            generalList <- generalList.addToEnd(concatenateLists);
            true;
        }
    };

    filterList(listIndex: Int, f: Filter): Bool {{
        generalList <- generalList.filterByAtSpecificPosition(f, listIndex);
        true;
    }};

    sortList(listIndex: Int, c: Comparator, order: String): Bool {{
        generalList <- generalList.sortByAtSpecificPosition(c, listIndex, order);
        true;
    }};

    checkIO(parameters: List): Bool {
        if not(parameters.length() = 0)
        then false
        else { currentList <- currentList.addToEnd(new IO); true; }
        fi
    };

    checkString(parameters: List): Bool {
        if not(parameters.length() = 1)
        then false
        else
            case parameters.head() of
                x : String => { 
                    currentList <- currentList.addToEnd(x);
                    true; 
                };
            esac
        fi
    };

    checkBool(parameters: List): Bool {
        if not(parameters.length() = 1)
        then false
        else
            case parameters.head() of
                x : String => (
                    if x = "true"
                    then
                        let newBool: Bool <- true
                        in {
                            currentList <- currentList.addToEnd(newBool);
                            true;
                        }
                    else
                        let newBool: Bool <- false
                        in {
                            currentList <- currentList.addToEnd(newBool);
                            true;
                        }
                    fi
                );
            esac
        fi
    };

    checkInt(parameters: List): Bool {
        if not(parameters.length() = 1)
        then false
        else
            case parameters.head() of
                x : String => {
                    currentList <- currentList.addToEnd((new A2I).a2i(x));
                    true;
                };
            esac
        fi
    };

    checkSoda(parameters: List): Bool {
        if not(parameters.length() = 3)
        then false
        else
            let firstParam: Object <- parameters.head(),
                secondParam: Object <- parameters.tail().head(),
                thirdParam: Object <- parameters.tail().tail().head()
            in 
                case firstParam of
                    x: String => (
                        case secondParam of
                            y: String => (
                                case thirdParam of
                                    z: String => {
                                        currentList <- currentList.addToEnd((new Soda).init(x, y, (new A2I).a2i(z)));
                                        true; 
                                    };
                                esac
                            );
                        esac
                    );
                esac
        fi
    };

    checkCoffee(parameters: List): Bool {
        if not(parameters.length() = 3)
        then false
        else
            let firstParam: Object <- parameters.head(),
                secondParam: Object <- parameters.tail().head(),
                thirdParam: Object <- parameters.tail().tail().head()
            in 
                case firstParam of
                    x: String => (
                        case secondParam of
                            y: String => (
                                case thirdParam of
                                    z: String => {
                                        currentList <- currentList.addToEnd((new Coffee).init(x, y, (new A2I).a2i(z)));
                                        true; 
                                    };
                                esac
                            );
                        esac
                    );
                esac
        fi
    };

    checkLaptop(parameters: List): Bool {
        if not(parameters.length() = 3)
        then false
        else
            let firstParam: Object <- parameters.head(),
                secondParam: Object <- parameters.tail().head(),
                thirdParam: Object <- parameters.tail().tail().head()
            in 
                case firstParam of
                    x: String => (
                        case secondParam of
                            y: String => (
                                case thirdParam of
                                    z: String => {
                                        currentList <- currentList.addToEnd((new Laptop).init(x, y, (new A2I).a2i(z)));
                                        true; 
                                    };
                                esac
                            );
                        esac
                    );
                esac
        fi
    };

    checkRouter(parameters: List): Bool {
        if not(parameters.length() = 3)
        then false
        else
            let firstParam: Object <- parameters.head(),
                secondParam: Object <- parameters.tail().head(),
                thirdParam: Object <- parameters.tail().tail().head()
            in 
                case firstParam of
                    x: String => (
                        case secondParam of
                            y: String => (
                                case thirdParam of
                                    z: String => {
                                        currentList <- currentList.addToEnd((new Router).init(x, y, (new A2I).a2i(z)));
                                        true; 
                                    };
                                esac
                            );
                        esac
                    );
                esac
        fi
    };

    checkPrivate(parameters: List): Bool {
        if not(parameters.length() = 1)
        then false
        else
            case parameters.head() of
                x : String => { 
                    currentList <- currentList.addToEnd((new Private).init(x));
                    true; 
                };
            esac
        fi
    };

    checkCorporal(parameters: List): Bool {
        if not(parameters.length() = 1)
        then false
        else
            case parameters.head() of
                x : String => { 
                    currentList <- currentList.addToEnd((new Corporal).init(x));
                    true; 
                };
            esac
        fi
    };

    checkSergent(parameters: List): Bool {
        if not(parameters.length() = 1)
        then false
        else
            case parameters.head() of
                x : String => { 
                    currentList <- currentList.addToEnd((new Sergent).init(x));
                    true; 
                };
            esac
        fi
    };

    checkOfficer(parameters: List): Bool {
        if not(parameters.length() = 1)
        then false
        else
            case parameters.head() of
                x : String => { 
                    currentList <- currentList.addToEnd((new Officer).init(x));
                    true; 
                };
            esac
        fi
    };

    inputLineValidation(currentLine: List): Bool {
        let currentLength: Int <- currentLine.length()
        in
            if currentLength < 1
            then false
            else
                case currentLine.head() of
                    x: String => (
                        let parameters: List <- currentLine.tail()
                        in (
                            if x = "IO"
                            then checkIO(parameters)
                            else
                                if x = "String"
                                then checkString(parameters)
                                else
                                    if x = "Bool"
                                    then checkBool(parameters)
                                    else
                                        if x = "Int"
                                        then checkInt(parameters)
                                        else
                                            if x = "Soda"
                                            then checkSoda(parameters)
                                            else
                                                if x = "Coffee"
                                                then checkCoffee(parameters)
                                                else
                                                    if x = "Laptop"
                                                    then checkLaptop(parameters)
                                                    else
                                                        if x = "Router"
                                                        then checkRouter(parameters)
                                                        else
                                                            if x = "Private"
                                                            then checkPrivate(parameters)
                                                            else
                                                                if x = "Corporal"
                                                                then checkCorporal(parameters)
                                                                else
                                                                    if x = "Sergent"
                                                                    then checkSergent(parameters)
                                                                    else
                                                                        if x = "Officer"
                                                                        then checkOfficer(parameters)
                                                                        else false
                                                                        fi
                                                                    fi
                                                                fi
                                                            fi
                                                        fi
                                                    fi
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
    };

    getGeneralList(): List {
        generalList
    };
};