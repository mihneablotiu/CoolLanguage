(*
   The class A2I provides integer-to-string and string-to-integer
conversion routines.  To use these routines, either inherit them
in the class where needed, have a dummy variable bound to
something of type A2I, or simpl write (new A2I).method(argument).
*)


(*
   c2i   Converts a 1-character string to an integer.  Aborts
         if the string is not "0" through "9"
*)
class A2I {

    c2i(char : String) : Int {
        if char = "0" then 0 else
        if char = "1" then 1 else
        if char = "2" then 2 else
            if char = "3" then 3 else
            if char = "4" then 4 else
            if char = "5" then 5 else
            if char = "6" then 6 else
            if char = "7" then 7 else
            if char = "8" then 8 else
            if char = "9" then 9 else
            { abort(); 0; }  -- the 0 is needed to satisfy the typchecker
            fi fi fi fi fi fi fi fi fi fi
     };

(*
   i2c is the inverse of c2i.
*)
     i2c(i : Int) : String {
	if i = 0 then "0" else
	if i = 1 then "1" else
	if i = 2 then "2" else
	if i = 3 then "3" else
	if i = 4 then "4" else
	if i = 5 then "5" else
	if i = 6 then "6" else
	if i = 7 then "7" else
	if i = 8 then "8" else
	if i = 9 then "9" else
	{ abort(); ""; }  -- the "" is needed to satisfy the typchecker
        fi fi fi fi fi fi fi fi fi fi
     };

(*
   a2i converts an ASCII string into an integer.  The empty string
is converted to 0.  Signed and unsigned strings are handled.  The
method aborts if the string does not represent an integer.  Very
long strings of digits produce strange answers because of arithmetic 
overflow.

*)
     a2i(s : String) : Int {
        if s.length() = 0 then 0 else
	if s.substr(0,1) = "-" then ~a2i_aux(s.substr(1,s.length()-1)) else
        if s.substr(0,1) = "+" then a2i_aux(s.substr(1,s.length()-1)) else
           a2i_aux(s)
        fi fi fi
     };

(*
  a2i_aux converts the usigned portion of the string.  As a programming
example, this method is written iteratively.
*)
     a2i_aux(s : String) : Int {
	(let int : Int <- 0 in	
           {	
               (let j : Int <- s.length() in
	          (let i : Int <- 0 in
		    while i < j loop
			{
			    int <- int * 10 + c2i(s.substr(i,1));
			    i <- i + 1;
			}
		    pool
		  )
	       );
              int;
	    }
        )
     };

(*
    i2a converts an integer to a string.  Positive and negative 
numbers are handled correctly.  
*)
    i2a(i : Int) : String {
	if i = 0 then "0" else 
        if 0 < i then i2a_aux(i) else
          "-".concat(i2a_aux(i * ~1)) 
        fi fi
    };
	
(*
    i2a_aux is an example using recursion.
*)		
    i2a_aux(i : Int) : String {
        if i = 0 then "" else 
	    (let next : Int <- i / 10 in
		i2a_aux(next).concat(i2c(i - next * 10))
	    )
        fi
    };

};
class List {
    isNil(): Bool {
        true
    };

    head(): Object {
        abort()
    };

    tail(): List {{
        abort();
        self;
    }};

    length(): Int {
        0
    };

    cons(o: Object): Cons {
        (new Cons).init(o, self)
    };

    addToEnd(o : Object): Cons {
        (new Cons).init(o, self)
    };

    getListFromPosition(position: Int): List {
        { abort(); self; }
    };

    removeListFromPosition(position: Int): List {
        { abort(); self; }
    };

    concatenateLists(secondList: List): List {
        secondList
    };

    toStringHelper(consNumber: Int): String {
        ""
    };

    toString(): String {
        toStringHelper(1)
    };

    filterBy(filter: Filter): List {
        self
    };

    filterByAtSpecificPosition(f: Filter, index: Int): List {
        self
    };

    insert(element: Object, order: String, c: Comparator): List {
        (new Cons).init(element, self)
    };

    sortBy(c: Comparator, order: String): List {
        self
    };

    sortByAtSpecificPosition(c: Comparator, index: Int, order: String): List {
        self
    };
};

class Cons inherits List {
    car: Object;
    cdr: List;

    init(newCar: Object, newCdr: List) : Cons {{
        car <- newCar;
        cdr <- newCdr;
        self;
    }};

    isNil(): Bool {
        false
    };

    head(): Object {
        car
    };

    tail(): List {
        cdr
    };

    length(): Int {
        1 + tail().length()
    };

    addToEnd(o: Object): Cons {
        tail().addToEnd(o).cons(head())
    };

    getListFromPosition(position: Int): List {
        if position = 0
        then
            case head() of
                x: Cons => x;
                x: List => x;
            esac
        else tail().getListFromPosition(position - 1)
        fi
    };

    removeListFromPosition(position: Int): List {
        if position = 0
        then cdr
        else tail().removeListFromPosition(position - 1).cons(car)
        fi
    };

    concatenateLists(secondList: List): List {
        if secondList.isNil()
        then self
        else self.addToEnd(secondList.head()).concatenateLists(secondList.tail())
        fi
    };

    printString(stringToPrint: String): String {
        if tail().length() = 0
        then "String(".concat(stringToPrint).concat(")").concat(tail().toString())
        else "String(".concat(stringToPrint).concat("), ").concat(tail().toString())
        fi
    };

    printIO(): String {
        if tail().length() = 0
        then "IO()".concat(tail().toString())
        else "IO(), ".concat(tail().toString())
        fi
    };

    printBool(boolToPrint: Bool): String {
        if tail().length() = 0
        then
            if boolToPrint = true
            then "Bool(true)".concat(tail().toString())
            else "Bool(false)".concat(tail().toString())
            fi
        else
            if boolToPrint = true
            then "Bool(true), ".concat(tail().toString())
            else "Bool(false), ".concat(tail().toString())
            fi
        fi
    };

    printInt(intToPrint: Int): String {
        if tail().length() = 0
        then "Int(".concat((new A2I).i2a(intToPrint)).concat(")").concat(tail().toString())
        else "Int(".concat((new A2I).i2a(intToPrint)).concat("), ").concat(tail().toString())
        fi
    };

    printCons(consToPrint: Cons, consNumber: Int): String {
        (new A2I).i2a(consNumber).concat(": [ ").concat(consToPrint.toString()).concat(" ]\n").concat(tail().toStringHelper(consNumber + 1))
    };

    toStringHelper(consNumber: Int): String {
        let tailLength: Int <- tail().length()
        in (
            case head() of
                x1: Cons => printCons(x1, consNumber);
                x2: String => printString(x2);
                x3: IO => printIO();
                x4: Bool => printBool(x4);
                x5: Int => printInt(x5);
                x6: Coffee => x6.toString(tailLength).concat(tail().toStringHelper(consNumber));
                x7: Soda => x7.toString(tailLength).concat(tail().toStringHelper(consNumber));
                x8: Laptop => x8.toString(tailLength).concat(tail().toStringHelper(consNumber));
                x9: Router => x9.toString(tailLength).concat(tail().toStringHelper(consNumber));
                x10: Private => x10.toString(tailLength).concat(tail().toStringHelper(consNumber));
                x11: Corporal => x11.toString(tailLength).concat(tail().toStringHelper(consNumber));
                x12: Sergent => x12.toString(tailLength).concat(tail().toStringHelper(consNumber));
                x13: Officer => x13.toString(tailLength).concat(tail().toStringHelper(consNumber));
            esac
        )
    };

    filterBy(f: Filter): List {
        let filterTail: List <- tail().filterBy(f)
        in 
            if f.filter(head())
            then filterTail.cons(head())
            else filterTail
            fi
    };

    filterByAtSpecificPosition(f: Filter, index: Int): List {
        if index = 0
        then
            case head() of
                x: List => tail().cons(x.filterBy(f));
            esac
        else tail().filterByAtSpecificPosition(f, index - 1).cons(head())
        fi
    };

    insert(element: Object, order: String, c: Comparator): List {
        if order = "ascendent"
        then
            if c.compareTo(element, head()) <= 0
            then self.cons(element)
            else tail().insert(element, order, c).cons(head())
            fi
        else
            if c.compareTo(element, head()) <= 0
            then tail().insert(element, order, c).cons(head())
            else self.cons(element)
            fi
        fi
    };

    sortBy(c: Comparator, order: String): List {
        tail().sortBy(c, order).insert(head(), order, c)
    };

    sortByAtSpecificPosition(c: Comparator, index: Int, order: String): List {
        if index = 0
        then
            case head() of
                x: List => tail().cons(x.sortBy(c, order));
            esac
        else tail().sortByAtSpecificPosition(c, index - 1, order).cons(head())
        fi
    };
};
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
};class Main inherits IO {
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
};(*******************************
 *** Classes Product-related ***
 *******************************)
class Product {
    name : String;
    model : String;
    price : Int;

    init(n : String, m: String, p : Int):SELF_TYPE {{
        name <- n;
        model <- m;
        price <- p;
        self;
    }};

    getprice():Int{ price * 119 / 100 };

    toString(tailLength: Int):String {
        if tailLength = 0
        then type_name().concat("(".concat(name.concat(";").concat(model)).concat(")"))
        else type_name().concat("(".concat(name.concat(";").concat(model)).concat("), "))
        fi
    };
};

class Edible inherits Product {
    -- VAT tax is lower for foods
    getprice():Int { price * 109 / 100 };
};

class Soda inherits Edible {
    -- sugar tax is 20 bani
    getprice():Int {price * 109 / 100 + 20};
};

class Coffee inherits Edible {
    -- this is technically poison for ants
    getprice():Int {price * 119 / 100};
};

class Laptop inherits Product {
    -- operating system cost included
    getprice():Int {price * 119 / 100 + 499};
};

class Router inherits Product {};

(****************************
 *** Classes Rank-related ***
 ****************************)
class Rank {
    name : String;

    init(n : String): SELF_TYPE {{
        name <- n;
        self;
    }};

    getHierarchy(): Int {{
        abort(); 
        100;
    }};

    toString(tailLength: Int):String {
        if tailLength = 0
        then type_name().concat("(".concat(name).concat(")"))
        else type_name().concat("(".concat(name).concat("), "))
        fi
    };
};

class Private inherits Rank {
    getHierarchy(): Int {
        1
    };
};

class Corporal inherits Private {
    getHierarchy(): Int {
        2
    };
};

class Sergent inherits Corporal {
    getHierarchy(): Int {
        3
    };
};

class Officer inherits Sergent {
    getHierarchy(): Int {
        4
    };
};class Tokenizer inherits IO {
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
class Comparator {
    compareTo(o1 : Object, o2 : Object):Int {0};
};

class PriceComparator inherits Comparator {
    compareTo(o1: Object, o2: Object): Int {
        let o1Price: Int, o2Price: Int, result: Int
        in {
            case o1 of
                x1: Soda => o1Price <- x1.getprice();
                x2: Coffee => o1Price <- x2.getprice();
                x3: Laptop => o1Price <- x3.getprice();
                x4: Router => o1Price <- x4.getprice();
                x5: Object => abort();
            esac;

            case o2 of
                x1: Soda => o2Price <- x1.getprice();
                x2: Coffee => o2Price <- x2.getprice();
                x3: Laptop => o2Price <- x3.getprice();
                x4: Router => o2Price <- x4.getprice();
                x5: Object => abort();
            esac;

            result <- o1Price - o2Price;
            result;
        }
    };
};

class RankComparator inherits Comparator {
    compareTo(o1: Object, o2: Object): Int {
        let o1Hierarchy: Int, o2Hierarchy: Int, result: Int
        in {
            case o1 of
                x1: Private => o1Hierarchy <- x1.getHierarchy();
                x2: Corporal => o1Hierarchy <- x2.getHierarchy();
                x3: Sergent => o1Hierarchy <- x3.getHierarchy();
                x4: Officer => o1Hierarchy <- x4.getHierarchy();
                x5: Object => abort();
            esac;

            case o2 of
                x1: Private => o2Hierarchy <- x1.getHierarchy();
                x2: Corporal => o2Hierarchy <- x2.getHierarchy();
                x3: Sergent => o2Hierarchy <- x3.getHierarchy();
                x4: Officer => o2Hierarchy <- x4.getHierarchy();
                x5: Object => abort();
            esac;

            result <- o1Hierarchy - o2Hierarchy;
            result;
        }
    };
};

class AlphabeticComparator inherits Comparator {
    compareTo(o1: Object, o2: Object): Int {
        let o1Value: String, o2Value: String, result: Int
        in {
            case o1 of
                x1: String => o1Value <- x1;
                x2: Object => abort();
            esac;

            case o2 of
                x1: String => o2Value <- x1;
                x2: Object => abort();
            esac;

            if o1Value < o2Value
            then result <- (1 - 2)
            else
                if o2Value < o1Value 
                then result <- 1
                else result <- 0
                fi
            fi;
            result;
        }
    };
};

class Filter {
    filter(o : Object):Bool {true};
};

class ProductFilter inherits Filter {
    filter(o: Object): Bool {
        let result: Bool <- false
        in {
            case o of
                x1: Soda => result <- true;
                x2: Coffee => result <- true;
                x3: Laptop => result <- true;
                x4: Router => result <- true;
                x5: Object => result <- false;
            esac;

            result;
        }
    };
};

class RankFilter inherits Filter {
    filter(o: Object): Bool {
        let result: Bool <- false
        in {
            case o of
                x1: Private => result <- true;
                x2: Corporal => result <- true;
                x3: Sergent => result <- true;
                x4: Officer => result <- true;
                x5: Object => result <- false;
            esac;

            result;
        }
    };
};

class SamePriceFilter inherits Filter {
    filter(o: Object): Bool {
        let result: Bool <- false
        in {
            case o of
                x1: Soda => result <- (x1.getprice() = x1@Product.getprice());
                x2: Coffee => result <- (x2.getprice() = x2@Product.getprice());
                x3: Laptop => result <- (x3.getprice() = x3@Product.getprice());
                x4: Router => result <- (x4.getprice() = x4@Product.getprice());
                x5: Object => result <- false;
            esac;

            result;
        }
    };
};