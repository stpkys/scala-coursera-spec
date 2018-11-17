/*
 * We have some hierarchy and the base trait is Base
 *
 * As you can see from the examples below our current approach has many drawbacks
 * (because the return type of changeVersion is not constrained):
 *   - it can return some other implementation of Base (see `Bar`)
 *   - it cannot be used in abstraction because the type is not specific enough (see `update` method)
 *
 * At some point we decided to constraint the return type of changeVersion method.
 * Instead of returning a base type (which is Base) we would like you to
 * make it type safe in a way, so that this method returns the type of the object
 * implementing that trait (and make it impossible to return any other type/subtype)
 *
 * There are different solutions to this problem and you can do whatever you want with
 * Base trait (split it, update it, remove it and use something else, etc) as long
 * as we are able to update a version in a type safe way returning the type of an object
 * which we are updating
 */

trait Base {
  def version: Integer
}

trait Versioned[Self]{
  self: Self =>
  def version: Integer
  def changeVersion(newVersion: Integer): Self
}

case class Foo(version: Integer) extends Base with Versioned[Foo] {
  def changeVersion(newVersion: Integer): Foo = copy(version = newVersion)
}

//case class Bar(version: Integer) extends Base with Versioned[Bar] {
//  def changeVersion(newVersion: Integer): Foo = Foo(newVersion) // oops :( it's not restricted to return Foo here
//}

def update[A <: Versioned[A]](a: A): A = a.changeVersion(a.version + 1) // compiler error// :(

1 + 1