function get_cache_filename()
    os.execute("mkdir -p /tmp/mpv-sceneseek")
    local handle = io.popen("nextname /tmp/mpv-sceneseek/mpv-sceneseek-#")
    local result = handle:read("*a")
    handle:close()
    return string.gsub(result, "\n", "")
end

-- Global variables
cache_filename = get_cache_filename()
threshold = 0.2


function seek_next_scene()
    next_scene = get_next_scene_position()
    if next_scene ~= nil then
        mp.set_property("time-pos", next_scene)
    end
end

function seek_prev_scene()
    prev_scene = get_prev_scene_position()
    if prev_scene ~= nil then
        mp.set_property("time-pos", prev_scene)
    end
end

function lower_threshold()
    threshold = threshold - 0.05
    invalidate_cache()
    mp.osd_message("Threshold: " .. threshold)
end

function raise_threshold()
    threshold = threshold + 0.05
    invalidate_cache()
    mp.osd_message("Threshold: " .. threshold)
end

function invalidate_cache()
    if file_exists(cache_filename) then
        os.execute("rm " .. cache_filename)
    end
end

function get_next_scene_position()
    current_pos = tonumber(mp.get_property("time-pos"))
    scenes = get_scene_positions()

    next_scene = nil
    for n,v in pairs(scenes) do
        if v > current_pos then
            next_scene = v
            break
        end
    end

    return next_scene
end

function get_prev_scene_position()
    current_pos = tonumber(mp.get_property("time-pos"))

    scenes = get_scene_positions()
    local rscenes = reverse_table(scenes)

    prev_scene = nil
    for n,v in pairs(rscenes) do
        if v < current_pos then
            prev_scene = v
            break
        end
    end

    return prev_scene
end


function get_scene_positions()
    print(cache_filename)
    if not file_exists(cache_filename) then
        calculate_scene_positions("0.2")
    end

    return lines_from(cache_filename)
end

function calculate_scene_positions(threshold)
    filename = mp.get_property("path")

    -- os.execute("ffmpeg -ss " .. pre_seek .. " -i \"" .. original_filename .. "\" -ss " .. post_seek .. " -to " .. post_seek_end .. " -qscale 0 \"" .. filename .. "\"")
    os.execute("scene-start-positions " .. filename .. " " .. threshold .. " > " .. cache_filename)
end

-- see if the file exists
function file_exists(file)
  local f = io.open(file, "r")
  if f then f:close() end
  return f ~= nil
end

-- get all lines from a file, returns an empty 
-- list/table if the file does not exist
function lines_from(file)
  if not file_exists(file) then return {} end
  f = io.open(file, "r")
  lines = {}
  for line in f:lines() do 
    lines[#lines + 1] = tonumber(line)
  end
  return lines
end


function reverse_table(t)
    local reversedTable = {}
    local itemCount = #t
    for k, v in ipairs(t) do
        reversedTable[itemCount + 1 - k] = v
    end
    return reversedTable
end

mp.add_key_binding("ctrl+]", seek_next_scene)
mp.add_key_binding("ctrl+[", seek_prev_scene)
mp.add_key_binding("ctrl+}", raise_threshold)
mp.add_key_binding("ctrl+{", lower_threshold)
