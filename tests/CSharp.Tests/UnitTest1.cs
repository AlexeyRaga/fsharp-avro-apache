namespace CSharp.Tests;

public class UnitTest1
{
    [Fact]
    public void Test1()
    {
        var foo = new global::Tests.Foo();
        foo.Age = 12;
        foo.Name = "foo";
    }
}
