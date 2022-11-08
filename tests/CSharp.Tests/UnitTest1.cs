using Avro;
using Avro.IO;
using Avro.Specific;
using CSharp.AvroMsg;

namespace CSharp.Tests;

public class UnitTest1
{
    public static byte[] Encode<T>(T value) where T: ISpecificRecord
    {
        var writer = new SpecificWriter<T>(value.Schema);
        using var ms = new MemoryStream();
        writer.Write(value, new BinaryEncoder(ms));
        return ms.ToArray();
    }

    public static T Decode<T>(Schema schema, byte[] bytes) where T : ISpecificRecord, new()
    {
        var reader = new SpecificReader<T>(schema, schema);
        using var ms = new MemoryStream(bytes);
        return reader.Read(new T(), new BinaryDecoder(ms));
    }

    [Fact]
    public void Test1()
    {
        var msg = new CSharp.AvroMsg.TestMessage
        {
            id = Guid.Empty,
            array = Array.Empty<string>(),
            str = "",
            choice = "",
            optional_choice = null,
            map = new Dictionary<string, bool>(),
            md5 = new CSharp.AvroMsg.MD5() { Value = new byte[16] },
            suit = Suit.SPADES
        };

        var bytes = Encode(msg);
        var decoded = Decode<CSharp.AvroMsg.TestMessage>(CSharp.AvroMsg.TestMessage._SCHEMA, bytes);
        Console.WriteLine(decoded);
    }
}
