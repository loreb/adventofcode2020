#! /usr/bin/awk -f

BEGIN {
        field["byr"] = "(Birth Year)"
        field["iyr"] = "(Issue Year)"
        field["eyr"] = "(Expiration Year)"
        field["hgt"] = "(Height)"
        field["hcl"] = "(Hair Color)"
        field["ecl"] = "(Eye Color)"
        field["pid"] = "(Passport ID)"
        field["cid"] = "(Country ID)"
        optional["cid"] = "optional in part 1"
        nvalid = 0 # to print zero instead of "" if nothing is valid
}

function reset() {
        for (f in field) {
                delete record[f]
        }
        endrecord = 1
}

func validheight(h) {
        # "180cm" - 66in
        # If cm, the number must be at least 150 and at most 193.
        # If in, the number must be at least 59 and at most 76.
        if (match(h, ".*cm$")) {
                h = substr(h, 1, length(h)-2)
                return h >= 150 && h <= 193
        }
        if (match(h, ".*in$")) {
                h = substr(h, 1, length(h)-2)
                return h >= 59 && h <= 76
        }
        return 0
}

function validhaircolor(color) {
        # a # followed by exactly six characters 0-9 or a-f
        return length(color) == 7 && match(color, "^#[0-9a-f]*$")
}

function valideyecolor(color) {
        # exactly one of: amb blu brn gry grn hzl oth
        split("amb blu brn gry grn hzl oth", eyecolors)
        #return eyecolors[color] :D
        for (idx in eyecolors) {
                if (eyecolors[idx] == color) {
                        return 1
                }
        }
        return 0
}

function validpassportid(id) {
        # a nine-digit number, including leading zeroes.
        return length(id) == 9 && match(id, "^[0-9]*$") > 0
}

# can't have an array of constraints in awk; oh well...
function constraintsok() {
        byr = record["byr"]
        iyr = record["iyr"]
        eyr = record["eyr"]
        hgt = record["hgt"]
        hcl = record["hcl"]
        ecl = record["ecl"]
        pid = record["pid"]
        cid = record["cid"]
        if (!(byr >= 1920 && byr <= 2002)) return 0
        if (!(iyr >= 2010 && iyr <= 2020)) return 0
        if (!(eyr >= 2020 && eyr <= 2030)) return 0
        if (!validheight(hgt)) return 0
        if (!validhaircolor(hcl)) return 0
        if (!valideyecolor(ecl)) return 0
        if (!validpassportid(pid)) return 0
        # ignore cid
        return 1
}

# Count the number of valid passports - those that have all required fields.
# Treat cid as optional.
function processrecord() {
        for (f in field) {
                if (!record[f]) {
                        if (!optional[f]) {
                                #print "missing " f
                                #for (x in record) { print("\t" x " " record[x]) }
                                return
                        }
                }
        }
        if (constraintsok()) {
                nvalid++
        }
}

NF == 0 {
        endrecord = 1
        processrecord()
        reset()
        print("\n")
}

NF > 0 {
        endrecord = 0
        for (i = 1; i <= NF; i++) {
                split($i, kv, ":")
                # kv[1] => field name; kv[2] => value
                record[kv[1]] = kv[2]
        }
}

END {
        if (!endrecord) {
                processrecord()
        }
        print nvalid
}
