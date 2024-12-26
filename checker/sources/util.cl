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