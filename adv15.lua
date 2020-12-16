
-- https://stackoverflow.com/questions/9168058/how-to-dump-a-table-to-console
function dump(o)
        if type(o) == 'table' then
                local s = '{ '
                for k,v in pairs(o) do
                        if type(k) ~= 'number' then k = '"'..k..'"' end
                        s = s .. '['..k..'] = ' .. dump(v) .. ','
                end
                return s .. '} '
        else
                return tostring(o)
        end
end

-- XXX can't do "arr = { loadfile(file) }"?

-- XXX part2 is the same as part 2 but with a bigger #iterations: 2020 => 30000000
-- ==> test15_436.txt becomes a misnomer :D

function playline(line, num)
        local latest = {}
        local turn = 0
        local spoken = "if you see this it's a bug"
        local dbg = function(n)
                assert(type(n) == "number")
                print("------------------------------------------------------")
                print("turn=", turn, "play=", n, "latest=",latest[n])
                print("latest", dump(latest))
        end

        --for token in string.gmatch(line, "[^,]") do ARGH I'M SUCH AN IDIOT!
        for token in string.gmatch(line, "[^,]+") do
                -- this took forever as a normal "print" wouldn't do...
                -- tnx to the dump function above, it seems that
                -- lua DOES convert numbers to strings as needed,
                -- BUT only WHEN FORCED
                -- ie here it's "0", not the number zero :D
                --
                -- also I was initially confused by the difference between
                -- the reading at the beginning and subsequent turns.
                turn = turn + 1
                local dafuk = token+0
                latest[dafuk] = turn
                print("DAFUK",dafuk)
                spoken = dafuk
        end
        print("turn=", turn, "initial=",dump(latest))
        -- 0,3,6 in input => no output; next they start by playing SIX, NOT ZERO!
        -- ie the last number in the input counts as if it was SPOKEN...
        while turn <= num do
                if latest[spoken] == nil then
                        latest[spoken] = turn
                        spoken = 0
                else
                        local tmp = turn - latest[spoken]
                        latest[spoken] = turn
                        spoken = tmp
                end
                turn=turn+1
                print("spoken",spoken,"at turn",turn)
        end
end

function playfile(filename)
        for line in io.lines(filename) do
                playline(line,2020)
                playline(line,30000000)
        end
end

for i,filename in pairs(arg) do
        if i > 0 then
                print(arg[i])
                playfile(arg[i])
                print("\n")
        end
end


