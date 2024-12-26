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
