// ------------------------------------------------------------------------------
// <auto-generated>
//    Generated by avrogen, version 1.11.1
//    Changes to this file may cause incorrect behavior and will be lost if code
//    is regenerated
// </auto-generated>
// ------------------------------------------------------------------------------
namespace CSharp.AvroMsg
{
	using System;
	using System.Collections.Generic;
	using System.Text;
	using global::Avro;
	using global::Avro.Specific;
	
	[global::System.CodeDom.Compiler.GeneratedCodeAttribute("avrogen", "1.11.1")]
	public partial class TestMessage : global::Avro.Specific.ISpecificRecord
	{
		public static global::Avro.Schema _SCHEMA = global::Avro.Schema.Parse(@"{""type"":""record"",""name"":""TestMessage"",""namespace"":""CSharp.AvroMsg"",""fields"":[{""name"":""id"",""type"":{""type"":""string"",""logicalType"":""uuid""}},{""name"":""num"",""type"":""int""},{""name"":""array"",""type"":{""type"":""array"",""items"":""string""}},{""name"":""optional_num"",""type"":[""null"",""int""]},{""name"":""str"",""type"":""string""},{""name"":""choice"",""type"":[""string"",""int"",""boolean""]},{""name"":""optional_choice"",""type"":[""null"",""string"",""int"",""boolean""]},{""name"":""map"",""type"":{""type"":""map"",""values"":""boolean""}},{""name"":""md5"",""type"":{""type"":""fixed"",""name"":""MD5"",""namespace"":""CSharp.AvroMsg"",""size"":16}},{""name"":""suit"",""type"":{""type"":""enum"",""name"":""Suit"",""namespace"":""CSharp.AvroMsg"",""symbols"":[""SPADES"",""HEARTS"",""DIAMONDS"",""CLUBS""]}},{""name"":""owner"",""type"":{""type"":""record"",""name"":""Person"",""namespace"":""CSharp.AvroMsg"",""fields"":[{""name"":""name"",""type"":""string""},{""name"":""age"",""type"":""int""}]}},{""name"":""contact"",""type"":[""null"",""Person""]},{""name"":""supervisor"",""type"":[""null"",""string"",""Person""]}]}");
		private System.Guid _id;
		private int _num;
		private IList<System.String> _array;
		private System.Nullable<System.Int32> _optional_num;
		private string _str;
		private object _choice;
		private object _optional_choice;
		private IDictionary<string,System.Boolean> _map;
		private CSharp.AvroMsg.MD5 _md5;
		private CSharp.AvroMsg.Suit _suit;
		private CSharp.AvroMsg.Person _owner;
		private CSharp.AvroMsg.Person _contact;
		private object _supervisor;
		public virtual global::Avro.Schema Schema
		{
			get
			{
				return TestMessage._SCHEMA;
			}
		}
		public System.Guid id
		{
			get
			{
				return this._id;
			}
			set
			{
				this._id = value;
			}
		}
		public int num
		{
			get
			{
				return this._num;
			}
			set
			{
				this._num = value;
			}
		}
		public IList<System.String> array
		{
			get
			{
				return this._array;
			}
			set
			{
				this._array = value;
			}
		}
		public System.Nullable<System.Int32> optional_num
		{
			get
			{
				return this._optional_num;
			}
			set
			{
				this._optional_num = value;
			}
		}
		public string str
		{
			get
			{
				return this._str;
			}
			set
			{
				this._str = value;
			}
		}
		public object choice
		{
			get
			{
				return this._choice;
			}
			set
			{
				this._choice = value;
			}
		}
		public object optional_choice
		{
			get
			{
				return this._optional_choice;
			}
			set
			{
				this._optional_choice = value;
			}
		}
		public IDictionary<string,System.Boolean> map
		{
			get
			{
				return this._map;
			}
			set
			{
				this._map = value;
			}
		}
		public CSharp.AvroMsg.MD5 md5
		{
			get
			{
				return this._md5;
			}
			set
			{
				this._md5 = value;
			}
		}
		public CSharp.AvroMsg.Suit suit
		{
			get
			{
				return this._suit;
			}
			set
			{
				this._suit = value;
			}
		}
		public CSharp.AvroMsg.Person owner
		{
			get
			{
				return this._owner;
			}
			set
			{
				this._owner = value;
			}
		}
		public CSharp.AvroMsg.Person contact
		{
			get
			{
				return this._contact;
			}
			set
			{
				this._contact = value;
			}
		}
		public object supervisor
		{
			get
			{
				return this._supervisor;
			}
			set
			{
				this._supervisor = value;
			}
		}
		public virtual object Get(int fieldPos)
		{
			switch (fieldPos)
			{
			case 0: return this.id;
			case 1: return this.num;
			case 2: return this.array;
			case 3: return this.optional_num;
			case 4: return this.str;
			case 5: return this.choice;
			case 6: return this.optional_choice;
			case 7: return this.map;
			case 8: return this.md5;
			case 9: return this.suit;
			case 10: return this.owner;
			case 11: return this.contact;
			case 12: return this.supervisor;
			default: throw new global::Avro.AvroRuntimeException("Bad index " + fieldPos + " in Get()");
			};
		}
		public virtual void Put(int fieldPos, object fieldValue)
		{
			switch (fieldPos)
			{
			case 0: this.id = (System.Guid)fieldValue; break;
			case 1: this.num = (System.Int32)fieldValue; break;
			case 2: this.array = (IList<System.String>)fieldValue; break;
			case 3: this.optional_num = (System.Nullable<System.Int32>)fieldValue; break;
			case 4: this.str = (System.String)fieldValue; break;
			case 5: this.choice = (System.Object)fieldValue; break;
			case 6: this.optional_choice = (System.Object)fieldValue; break;
			case 7: this.map = (IDictionary<string,System.Boolean>)fieldValue; break;
			case 8: this.md5 = (CSharp.AvroMsg.MD5)fieldValue; break;
			case 9: this.suit = (CSharp.AvroMsg.Suit)fieldValue; break;
			case 10: this.owner = (CSharp.AvroMsg.Person)fieldValue; break;
			case 11: this.contact = (CSharp.AvroMsg.Person)fieldValue; break;
			case 12: this.supervisor = (System.Object)fieldValue; break;
			default: throw new global::Avro.AvroRuntimeException("Bad index " + fieldPos + " in Put()");
			};
		}
	}
}