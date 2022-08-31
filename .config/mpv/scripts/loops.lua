loops = {}
current_loop = 1
looping = false

function load_times()
    require 'mp.options'
    -- TODO:
    --  - Actually load the times here
    --  - Call this function when mpv is loaded
    --  - Usage is mpv ... --script-opts=loops-loadtimes=filename
    --      where filename is a scenes-file saved with CTRL+d/D
    options = { loadtimes = "" }
    read_options(options)
    print('test')
    print(options.loadtimes)
end

function next_loop()
    select_loop(current_loop + 1)
end

function prev_loop()
    select_loop(current_loop - 1)
end

function select_loop(n)
    current_loop = n
    mp.osd_message("Selected loop " .. current_loop .. "(" .. repr_loop(current_loop) .. ")")
end

function set_point()
    local current_time = mp.get_property("time-pos")
    local saved_points = len(loops[current_loop])

    if saved_points == 0 then
        loops[current_loop] = { current_time }
    elseif saved_points == 1 then
        mp.osd_message(loops[current_loop][1])
        table.insert(loops[current_loop], current_time)
    else
        loops[current_loop] = nil
    end

    mp.osd_message(repr_loop(current_loop))
end

function jump_a()
    local time_a = get_a(current_loop)
    if time_a ~= nil then
        mp.set_property("time-pos", time_a)
    end
end

function jump_b()
    local time_b = get_b(current_loop)
    if time_b ~= nil then
        mp.set_property("time-pos", time_b)
    end
end

function toggle_loop()
    if looping then
        mp.unobserve_property(do_loop)
        looping = false
        mp.osd_message("Looping off")
    else
        mp.observe_property("time-pos", "number", do_loop)
        looping = true
        mp.osd_message("Looping on")
    end
end

function do_loop(property_name, current_time)
    time_b = tonumber(get_b(current_loop))

    if time_b ~= nil and current_time > time_b then
        jump_a()
    end
end

function save_current_loop()
    save_loop(current_loop)
end

function save_current_time()
    save_time(current_loop)
end

function save_loops()
    for i in pairs(loops) do
        save_loop(i)
    end
end

function save_times()
    for i in pairs(loops) do
        save_time(i)
    end
end

-- Helpers

function save_loop(loop)
    local timeA = get_a(loop)
    local timeB = get_b(loop)

    original_filename = mp.get_property("path")
    if tonumber(timeB) == nil or tonumber(timeB) < 0 then
        mp.osd_message("Invalid loop!")
        return
    end

    filename = get_filename()
    mp.osd_message("Saving AB-Loop to " .. filename)

    time_a = tonumber(timeA)
    time_b = tonumber(timeB)

    pre_seek = .9 * time_a
    post_seek = time_a - pre_seek
    post_seek_end = time_b - pre_seek

    -- if (is_url(original_filename)) then
    --     original_filename = download_video(original_filename);
    -- end
    os.execute("ffmpeg -ss " .. pre_seek .. " -i \"" .. original_filename .. "\" -ss " .. post_seek .. " -to " .. post_seek_end .. " -qscale 0 \"" .. filename .. "\"")
    mp.osd_message("File saved!")
end

-- function is_url(s)
--     return string.sub(s, 0, 4) == "http"
-- end

function download_video(url)
    os.execute("mkdir -p /tmp/mpv-youtubedl")
    new_filename = get_youtubedl_filename(url)
    os.execute("youtube-dl \"" .. url .. "\" -o '/tmp/mpv-youtubedl/%(title)s-%(id)s.%(ext)s' -f best")
    return new_filename
end

function get_youtubedl_filename(url)
    local handle = io.popen("youtube-dl -f best --get-filename \"" .. url .. "\" -o '/tmp/mpv-youtubedl/%(title)s-%(id)s.%(ext)s'")
    local result = handle:read("*a")
    handle:close()
    return string.gsub(result, "\n", "")
end

function save_time(loop)
    -- TODO: Prevent overwriting the scenes file
    local timeA = get_a(loop)
    local timeB = get_b(loop)

    if tonumber(timeB) == nil or tonumber(timeB) < 0 then
        mp.osd_message("Invalid loop!")
    else
        os.execute("echo " .. timeA .. " " .. timeB .. " >> scenes")
        mp.osd_message("Times saved in ./scenes!")
    end
end

function get_a(loop)
    local tbl = loops[loop]
    if tbl == nil then
        return nil
    else
        return tbl[1]
    end
end

function get_b(loop)
    local tbl = loops[loop]
    if tbl == nil then
        return nil
    else
        return tbl[2]
    end
end

function len(tbl)
    if tbl == nil then
        return 0
    end

    local count = 0
    for _ in pairs(tbl) do count = count + 1 end
    return count
end

function repr_loop(loop)
    local num_points = len(loops[loop])
    if num_points == 0 then
        return "empty"
    elseif num_points == 1 then
        return "A.."
    else
        return "A..B"
    end
end

function get_filename()
    local handle = io.popen("nextname scene-#.mkv")
    local result = handle:read("*a")
    handle:close()
    return string.gsub(result, "\n", "")
end

function func_select_loop(n)
   return function()
       return select_loop(n)
   end
end

mp.add_key_binding("ctrl+n", next_loop)
mp.add_key_binding("ctrl+p", prev_loop)
mp.add_key_binding("ctrl+g", set_point)
mp.add_key_binding("ctrl+a", jump_a)
mp.add_key_binding("ctrl+b", jump_b)
mp.add_key_binding("ctrl+l", toggle_loop)
mp.add_key_binding("ctrl+s", save_current_loop)
mp.add_key_binding("ctrl+d", save_current_time)
mp.add_key_binding("ctrl+S", save_loops)
mp.add_key_binding("ctrl+D", save_times)
mp.add_key_binding("ctrl+1", func_select_loop(1))
mp.add_key_binding("ctrl+2", func_select_loop(2))
mp.add_key_binding("ctrl+3", func_select_loop(3))
mp.add_key_binding("ctrl+4", func_select_loop(4))
mp.add_key_binding("ctrl+5", func_select_loop(5))
mp.add_key_binding("ctrl+6", func_select_loop(6))
mp.add_key_binding("ctrl+7", func_select_loop(7))
mp.add_key_binding("ctrl+8", func_select_loop(8))
mp.add_key_binding("ctrl+9", func_select_loop(9))
