-- tilemap-exporter.lua written by Switch by modifying export.lua

-- Original file "export.lua" provided by David Capello
-- Copyright (C) 2020  David Capello
--
-- This file is released under the terms of the MIT license.

local spr = app.activeSprite
if not spr then return print "No active sprite" end

if ColorMode.TILEMAP == nil then ColorMode.TILEMAP = 4 end
assert(ColorMode.TILEMAP == 4)

local fs = app.fs
local pc = app.pixelColor
local output_folder = fs.fileTitle(spr.filename)
local image_n = 0
local tileset_n = 0

local function export_tileset(tileset)
  local t = {}
  local grid = tileset.grid
  local size = grid.tileSize
  local numberOfColumns = 8 -- You can choose your own number of columns here.
  t.grid = { tileSize={ width=grid.tileSize.width, height=grid.tileSize.height } }
  if #tileset > 0 then
    local spec = spr.spec
    spec.width = numberOfColumns * size.width
    spec.height = math.ceil(#tileset/size.height) * size.height
    local image = Image(spec)
    image:clear()

    local gridHeight = 0
    for i = 0,#tileset-1 do
      local tile = tileset:getTile(i)
      image:drawImage(tile, (i % numberOfColumns) * size.width, gridHeight)
	  
	  if (i+1) % numberOfColumns == 0 then
	    gridHeight = gridHeight + size.height
		  end
    end

    tileset_n = tileset_n + 1
    local imageFn = fs.joinPath(output_folder, "tileset" .. tileset_n .. ".png")
    image:saveAs(imageFn)
    t.image = imageFn
  end
  return t
end

local function export_tilesets(tilesets)
  local t = {}
  for _,tileset in ipairs(tilesets) do
    table.insert(t, export_tileset(tileset))
  end
  return t
end

local function get_tileset_index(layer)
  for i,tileset in ipairs(layer.sprite.tilesets) do
    if layer.tileset == tileset then
      return i-1
    end
  end
  return -1
end

----------------------------------------------------------------------
-- Creates output folder

fs.makeDirectory(output_folder)

----------------------------------------------------------------------
-- Write /sprite.json file in the output folder

local jsonFn = fs.joinPath(output_folder, "sprite.json")
local data = {
  filename=spr.filename,
  width=spr.width,
  height=spr.height,
}
if pcall(function() return spr.tilesets end) then
  data.tilesets = export_tilesets(spr.tilesets)
end

