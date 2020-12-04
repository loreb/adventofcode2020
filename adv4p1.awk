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
}

function reset() {
        for (f in field) {
                delete record[f]
        }
        endrecord = 1
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
        nvalid++
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
