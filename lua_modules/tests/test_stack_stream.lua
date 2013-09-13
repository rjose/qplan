StackStream = require('stack_stream')
require('string_utils')

TestStackStream = {}

string1 = [[=====test1
	First line
	Second line]]

string2 = [[=====test2-1
	First line
=====test2-2]]

string3 = [[=====test3
	=====test-nest 1
		Nested line 1
	=====test-nest 2
		Nested line 2]]

function TestStackStream:test_unstack()
        streams = StackStream.unstack(string1:split("\n"))
        assertEquals(#streams, 1)

        stream1 = streams[1]
        assertEquals(stream1.header, "test1")
        assertEquals(#stream1.lines, 2)
        assertEquals(stream1.lines[1], "First line")
end

function TestStackStream:test_unstack2()
        streams = StackStream.unstack(string2:split("\n"))
        assertEquals(#streams, 2)

        stream1 = streams[1]
        stream2 = streams[2]

        assertEquals(stream1.header, "test2-1")
        assertEquals(stream2.header, "test2-2")

        assertEquals(stream1.lines[1], "First line")
end


function TestStackStream:test_nested_streams()
        streams = StackStream.unstack(string3:split("\n"))
        assertEquals(#streams, 1)

        nested_streams = StackStream.unstack(streams[1].lines)
        assertEquals(#nested_streams, 2)
        nested_stream2 = nested_streams[2]

        assertEquals(nested_stream2.header, "test-nest 2")
        assertEquals(nested_stream2.lines[1], "Nested line 2")
end

