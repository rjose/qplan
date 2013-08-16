local func = {}


--==============================================================================
-- Public API
--

--------------------------------------------------------------------------------
-- Adds two values treating nil as 0.
--
function func.add(w1, w2)
        w1 = w1 or 0
        w2 = w2 or 0
	return w1 + w2
end


--------------------------------------------------------------------------------
-- Applies filter(s) to an item.
--
function func.apply_filter(filter, item)
        -- Case 1: filter is nil
        if filter == nil then return true end

        -- Case 2: filter is a single function
        if type(filter) == "function" then
                return filter(item)
        end

        -- Case 3: array of filters
        if type(filter) == "table" then
                local result = true
                for _, f in ipairs(filter) do
                        result = result and f(item)
                end
                return result
        end

        -- Case 4: Something else
       io.stderr:write("Unknown filter type to apply_filter\n")
       return nil
end


--------------------------------------------------------------------------------
-- Applies a function of 2 args key-wise to values in two tables.
--
-- NOTE: The function f should handle nil values in a way that makes sense.
--
function func.apply_keywise_2(f, t1, t2)
        t1 = t1 or {}
        t2 = t2 or {}

        local keys = func.key_union(t1, t2)

	local result = {}
        for _, key in pairs(keys) do
                result[key] = f(t1[key], t2[key])
        end

        return result
end


--------------------------------------------------------------------------------
-- Concatenates elements of arrays.
--
function func.concat(...)
        local arrays = {...}
        local result = {}

        for i = 1,#arrays do
                local a = arrays[i]
                for j = 1,#a do
                        result[#result+1] = a[j]
                end
        end

        return result
end


--------------------------------------------------------------------------------
-- Returns an array of items passing a filter.
--
function func.filter(items, filter)
	local result = {}
	for _, item in ipairs(items) do
                if func.apply_filter(filter, item) then
			result[#result+1] = item
		end
	end
	return result
end


--------------------------------------------------------------------------------
-- Returns all the keys in a table.
--
function func.get_table_keys(t)
	local result = {}
	for k, _ in pairs(t) do
		result[#result+1] = k .. ""
	end
        table.sort(result)
	return result
end



--------------------------------------------------------------------------------
-- Groups items into buckets defined by applying "get_bucket" to each one.
--
function func.group_items(items, get_bucket)
	local groupings = {}

        for _, item in ipairs(items) do
		local bucket = get_bucket(item) .. ""
		if not bucket then
			bucket = "??"
		end

                -- Put stuff into the bucket list :-)
		groupings[bucket] = groupings[bucket] or {}
                local bucket_list = groupings[bucket]
		bucket_list[#bucket_list+1] = item
	end

	-- Sort buckets
	local bucket_names = func.get_table_keys(groupings)
	table.sort(bucket_names)

        return groupings, bucket_names
end


--------------------------------------------------------------------------------
-- Returns an array of keys being the union of t1 and t2.
--
function func.key_union(...)
        local result = {}
        local keymap = {}
        for _, t in pairs({...}) do
                for k, _ in pairs(t) do
                        keymap[k] = true
                end
        end

        for k, _ in pairs(keymap) do
                result[#result+1] = k
        end

        return result
end



--------------------------------------------------------------------------------
-- Maps a function over the items in a table.
--
function func.map_table(f, t)
	local result = {}
	for k, item in pairs(t) do
                result[k] = f(item)
	end
	return result
end


--------------------------------------------------------------------------------
-- Returns shallow copy of table.
--
function func.shallow_copy(src)
        local result = {}
        for k, v in pairs(src) do
                result[k] = v
        end
        return result
end


--------------------------------------------------------------------------------
-- Splits an array into 2 arrays at a given index.
--
function func.split_at(n, a)
        local result1, result2 = {}, {}
        if n > #a then n = #a end

        for i = 1,n do
                result1[#result1+1] = a[i]
        end
        for i = n+1, #a do
                result2[#result2+1] = a[i]
        end
        return result1, result2
end


--------------------------------------------------------------------------------
-- Subtracts two values treating nil as 0.
--
function func.subtract(w1, w2)
        w1 = w1 or 0
        w2 = w2 or 0
	return w1 - w2
end


--------------------------------------------------------------------------------
-- Accumulates values from a table
--
-- TODO: Rewrite this function to match key_union
function func.value_union(acc, table)
        for _, val in pairs(table) do
                acc[val] = 1
        end
        return acc
end


return func
