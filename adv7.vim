" Vimscript is the worst possible language to accomplish anything.
" Second worst if you include batch scripts from windows.
"
" Example Rules:
"
" light red bags contain 1 bright white bag, 2 muted yellow bags.
" dark orange bags contain 3 bright white bags, 4 muted yellow bags.
" bright white bags contain 1 shiny gold bag.
" muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
" shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
" dark olive bags contain 3 faded blue bags, 4 dotted black bags.
" vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
" faded blue bags contain no other bags.
" dotted black bags contain no other bags.

" XXX string1 + string2 == 0; string concatenation is ".", as in perl!

function! s:isbag(b)
        let spellings = [ "bag,", "bags,", "bag.", "bags." ]
        for s in spellings
                if s == a:b
                        return 1
                endif
        endfor
        return 0
endfunction

" return a list of rules as a hash, eg:
" { "light red" => [ [1, "bright white"], [2, "muted yellow"] ] }
function! Parsefile(filename)
        let ret = {}
        for line in readfile(a:filename)
                let words = split(line)
                if len(words) < 7
                        throw "short line " . line
                endif
                let c = words[0] . " " . words[1] " color
                let a = [] " TODO decent names
                if has_key(ret, c)
                        throw "duplicate " . c
                endif
                if words[2] != "bags"
                        throw "no bags in " . line
                endif
                if words[3] != "contain"
                        throw "no contain in " . line
                endif
                let i = 4
                while i < len(words)
                        if words[i] == "no"
                                let a += [[0, "no other bag"]]
                                break
                        endif
                        let n = str2nr(words[i])
                        if n == 0
                                throw words[i] . " -- " . line
                        endif
                        let icolor = words[i+1] . " " . words[i+2]
                        let a += [[ n, icolor ]]
                        if !s:isbag(words[i+3])
                                throw "bags? " . words[i+3] . " -- line: " . line
                        endif
                        let i += 4
                endwhile
                let ret[c] = a
        endfor
        return ret
endfunction

function s:canreach(destination, from, rules)
        "echomsg printf("canreach %s from %s?", a:destination, a:from)
        if a:destination == a:from
                return 1
        endif
        for succ in a:rules[a:from]
                " [ n, color ]
                if succ[0] > 0 && s:canreach(a:destination, succ[1], a:rules)
                        return 1
                endif
        endfor
        return 0
endfunction

" shiny gold bags contain 2 dark red bags.
" dark red bags contain 2 dark orange bags.
" dark orange bags contain 2 dark yellow bags.
" dark yellow bags contain 2 dark green bags.
" dark green bags contain 2 dark blue bags.
" dark blue bags contain 2 dark violet bags.
" dark violet bags contain no other bags.
" => a shiny gold contains 126 other bags
function s:bagscontained(bagcolor, rules)
        let tot = 0
        for succ in a:rules[a:bagcolor]
                let n = succ[0]
                let c = succ[1]
                if n > 0
                        "echomsg printf("%s has %d %s", a:bagcolor, n, c)
                        "let tot += n
                        let tot += n * s:bagscontained(c, a:rules)
                endif
        endfor
        return tot == 0 ? 1 : tot + 1
endfunction

for f in glob("test7*",0,1) + glob("input7*",0,1)
        echomsg f
        " no glob => return [] instead of "test7*" :D
        " but need silly flags to return a list instead of a string...
        "echomsg Parsefile(f)
        let rules = Parsefile(f)
        let shiny = []
        for k in keys(rules)
                if k != "shiny gold" && s:canreach("shiny gold", k, rules)
                        let shiny += [ k ]
                endif
        endfor
        "echomsg shiny
        echomsg len(shiny)
        " minus one because it says OTHER bags, not including the shiny bag
        " itself :D
        echomsg printf("%d bags contained in a shiny gold bag", s:bagscontained("shiny gold", rules)-1)
        echomsg
endfor
