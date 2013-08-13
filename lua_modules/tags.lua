local func = require('functional')

Tags = {}

function parse_tags(tag_string)
	local result = {}

	if not tag_string then return result end

	-- First split on multiple tags
	tags = tag_string:split(",")
	for _, str in pairs(tags) do
		local tag, value = unpack(str:split(":"))
		result[tag] = value
	end

	return result
end
Tags.parse_tags = parse_tags

function tags_to_string(tags, sep)
        if not tags then
                return ""
        end

	sep = sep or ","

	local keys = func.get_table_keys(tags)
	table.sort(keys)


        local result = ""
	for _, key in ipairs(keys) do
                if tags[key] and tags[key] ~= '' then
                        result = result .. string.format("%s:%s" .. sep, key, tags[key])
                end
        end

        -- Strip trailing comma
        return result:sub(1, -(1 + string.len(sep)))
end
Tags.tags_to_string = tags_to_string

return Tags
