val filename = "/1975.csv"
val pattern = "([0-9]+)".r
val r = pattern.findFirstIn(filename).get