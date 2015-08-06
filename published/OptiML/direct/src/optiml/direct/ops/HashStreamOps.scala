package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait HashStreamOps extends Base {
  this: OptiML => 

  object HashStream {
    def apply[V:Typ](table: Rep[String],deserialize: (Rep[HashStream[V]],Rep[String]) => Rep[V])(implicit __pos: SourceContext,__imp1: Overload20) = hashstream_object_apply[V](table,deserialize)(implicitly[Typ[V]],__pos)
  }

  implicit def repToHashStreamHashStreamOpsCls[V:Typ](x: Rep[HashStream[V]])(implicit __pos: SourceContext) = new HashStreamHashStreamOpsCls(x)(implicitly[Typ[V]],__pos)
  implicit def varToHashStreamHashStreamOpsCls[V:Typ](x: Var[HashStream[V]])(implicit __pos: SourceContext) = new HashStreamHashStreamOpsCls(readVar(x))(implicitly[Typ[V]],__pos)

  class HashStreamHashStreamOpsCls[V:Typ](val self: Rep[HashStream[V]])(implicit __pos: SourceContext) {
    def open()(implicit __pos: SourceContext) = hashstream_open[V](self)(implicitly[Typ[V]],__pos)
    def apply(__arg1: Rep[String])(implicit __pos: SourceContext,__imp1: Overload23) = hashstream_apply[V](self,__arg1)(implicitly[Typ[V]],__pos)
    def contains(__arg1: Rep[String])(implicit __pos: SourceContext,__imp1: Overload8) = hashstream_contains[V](self,__arg1)(implicitly[Typ[V]],__pos)
    def keys(implicit __pos: SourceContext,__imp1: Overload3) = hashstream_keys[V](self)(implicitly[Typ[V]],__pos)
    def get(__arg1: Rep[String])(implicit __pos: SourceContext,__imp1: Overload3) = hashstream_get[V](self,__arg1)(implicitly[Typ[V]],__pos)
    def getRange(__arg1: Rep[String],__arg2: Rep[Int])(implicit __pos: SourceContext) = hashstream_getrange[V](self,__arg1,__arg2)(implicitly[Typ[V]],__pos)
    def put(__arg1: Rep[String],__arg2: Rep[ForgeArray[Byte]])(implicit __pos: SourceContext,__imp1: Overload3) = hashstream_put[V](self,__arg1,__arg2)(implicitly[Typ[V]],__pos)
    def putAll(__arg1: Rep[ForgeArray[String]],__arg2: Rep[ForgeArray[ForgeArray[Byte]]],__arg3: Rep[Int])(implicit __pos: SourceContext) = hashstream_putall[V](self,__arg1,__arg2,__arg3)(implicitly[Typ[V]],__pos)
    def close()(implicit __pos: SourceContext) = hashstream_close[V](self)(implicitly[Typ[V]],__pos)
    def mapValues[R:Typ](outFile: Rep[String],outDelim: Rep[String] = unit("    "))(func: (Rep[String],Rep[V]) => Rep[DenseVector[R]])(implicit __pos: SourceContext,__imp0: Stringable[R]) = hashstream_mapvalues[V,R](self,outFile,outDelim,func)(implicitly[Typ[V]],implicitly[Typ[R]],__pos,__imp0)
  }



  def hashstream_object_apply[V:Typ](table: Rep[String],deserialize: (Rep[HashStream[V]],Rep[String]) => Rep[V])(implicit __pos: SourceContext): Rep[HashStream[V]]
  def hashstream_open[V:Typ](self: Rep[HashStream[V]])(implicit __pos: SourceContext): Rep[Unit]
  def hashstream_apply[V:Typ](self: Rep[HashStream[V]],__arg1: Rep[String])(implicit __pos: SourceContext): Rep[V]
  def hashstream_contains[V:Typ](self: Rep[HashStream[V]],__arg1: Rep[String])(implicit __pos: SourceContext): Rep[Boolean]
  def hashstream_keys[V:Typ](self: Rep[HashStream[V]])(implicit __pos: SourceContext): Rep[ForgeArray[String]]
  def hashstream_get[V:Typ](self: Rep[HashStream[V]],__arg1: Rep[String])(implicit __pos: SourceContext): Rep[ForgeArray[Byte]]
  def hashstream_getrange[V:Typ](self: Rep[HashStream[V]],__arg1: Rep[String],__arg2: Rep[Int])(implicit __pos: SourceContext): Rep[ForgeArray[ForgeArray[Byte]]]
  def hashstream_put[V:Typ](self: Rep[HashStream[V]],__arg1: Rep[String],__arg2: Rep[ForgeArray[Byte]])(implicit __pos: SourceContext): Rep[Unit]
  def hashstream_putall[V:Typ](self: Rep[HashStream[V]],__arg1: Rep[ForgeArray[String]],__arg2: Rep[ForgeArray[ForgeArray[Byte]]],__arg3: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def hashstream_close[V:Typ](self: Rep[HashStream[V]])(implicit __pos: SourceContext): Rep[Unit]
  def hashstream_mapvalues[V:Typ,R:Typ](self: Rep[HashStream[V]],outFile: Rep[String],outDelim: Rep[String] = unit("    "),func: (Rep[String],Rep[V]) => Rep[DenseVector[R]])(implicit __pos: SourceContext,__imp0: Stringable[R]): Rep[FileStream]
}
trait HashStreamCompilerOps extends HashStreamOps {
  this: OptiML => 

  def hash_alloc_raw[V:Typ](table: Rep[String],deserialize: (Rep[HashStream[V]],Rep[String]) => Rep[V])(implicit __pos: SourceContext): Rep[HashStream[V]]
  def hash_open_internal(__arg0: Rep[String])(implicit __pos: SourceContext): Rep[org.rocksdb.RocksDB]
  def hash_get_internal(__arg0: Rep[org.rocksdb.RocksDB],__arg1: Rep[ForgeArray[Byte]])(implicit __pos: SourceContext): Rep[ForgeArray[Byte]]
  def hash_get_range_internal(__arg0: Rep[org.rocksdb.RocksDB],__arg1: Rep[ForgeArray[Byte]],__arg2: Rep[Int])(implicit __pos: SourceContext): Rep[ForgeArray[ForgeArray[Byte]]]
  def hash_put_internal(__arg0: Rep[org.rocksdb.RocksDB],__arg1: Rep[ForgeArray[Byte]],__arg2: Rep[ForgeArray[Byte]])(implicit __pos: SourceContext): Rep[Unit]
  def hash_put_all_internal(__arg0: Rep[org.rocksdb.RocksDB],__arg1: Rep[ForgeArray[ForgeArray[Byte]]],__arg2: Rep[ForgeArray[ForgeArray[Byte]]],__arg3: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def hash_close_internal(__arg0: Rep[org.rocksdb.RocksDB])(implicit __pos: SourceContext): Rep[Unit]
  def hash_keys_internal(__arg0: Rep[org.rocksdb.RocksDB])(implicit __pos: SourceContext): Rep[ForgeArray[String]]
  def hash_deserialize[V:Typ](self: Rep[HashStream[V]])(implicit __pos: SourceContext): Rep[Function1[Tup2[HashStream[V],String],V]]
  def hash_table_name[V:Typ](self: Rep[HashStream[V]])(implicit __pos: SourceContext): Rep[String]
  def hash_get_db[V:Typ](self: Rep[HashStream[V]])(implicit __pos: SourceContext): Rep[org.rocksdb.RocksDB]
  def hash_set_db[V:Typ](self: Rep[HashStream[V]],__arg1: Rep[org.rocksdb.RocksDB])(implicit __pos: SourceContext): Rep[Unit]
}

