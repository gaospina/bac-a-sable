/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/
package oscar.cbls.lib.invariant.logic

import oscar.cbls.core.computation.{CBLSSetVar, ChangingIntValue, Domain, IntValue, Invariant, InvariantHelper, ShortIntNotificationTarget, Store}
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.{SortedMap, SortedSet}

/**This package proposes a set of logic invariants, which are used to define the structure of the problem*/

/**maintains a cluster of the indexes of array:  cluster(j) = {i in index of values | values[i] == j}
  * This is considered as a sparse cluster because Cluster is a map and must not cover all possibles values of the values in the array ''values''
  * @author renaud.delandtsheer@cetic.be
  * */
case class SparseCluster(values:Array[IntValue], clusters:SortedMap[Int,CBLSSetVar])
  extends Invariant
  with ShortIntNotificationTarget{

  for (v <- values.indices) registerStaticAndDynamicDependency(values(v),v)

  finishInitialization()

  for(c <- clusters.values){c.setDefiningInvariant(this); c.setValue(SortedSet.empty)}

  for(v <- values.indices){
    val x:CBLSSetVar = clusters.getOrElse(values(v).valueInt,null)
    if(x != null) x.insertValue(v)
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Int, NewVal: Int): Unit = {
    val x:CBLSSetVar = clusters.getOrElse(OldVal,null)
    if(x != null) x.deleteValue(index)
    val y:CBLSSetVar = clusters.getOrElse(NewVal,null)
    if(y != null) y.insertValue(index)
  }

  override def checkInternals(c:Checker): Unit ={
    for(v <- values.indices){
      if (clusters.isDefinedAt(values(v).valueInt)) {
        c.check(clusters(values(v).valueInt).value.contains(v),
          Some(s"Clusters(values(v ($v)).value (${values(v).value})).value.contains(v)"))
      }
    }
    for(value <- clusters.keys){
      for (indices <- clusters(value).value){
        c.check(values(indices).value == value,
          Some(s"values(indices).value (${values(indices).value}) == value ($value)"))
      }
    }
  }
}

/**Maintains a cluster of the indexes of array: cluster(j) = {i in index of values | values[i] == j}
  * This is considered as a dense cluster because Cluster is an array and must cover all the possibles values of the values in the array ''values''
  * @author renaud.delandtsheer@cetic.be
  * */
case class DenseCluster(values:Array[IntValue], clusters:Array[CBLSSetVar]) extends Invariant with ShortIntNotificationTarget{

  //We register the static and dynamic dependencies.
  //Dynamic dependencies are the ones considered for the notifications.
  //Static dependencies are the ones considered for ordering the propagations
  for (v <- values.indices) registerStaticAndDynamicDependency(values(v),v)

  //This must be called once all static dependencies are registered
  //It must be called before the output dependencies are notified
  finishInitialization()

  //We then define the variable that we control
  //By theway, an initial value is set to each of them (SortedSet.empty)
  for(c <- clusters){
    c.setDefiningInvariant(this) //A variable can only have a single controlling invariant
    c.setValue(SortedSet.empty)
  }

  //We then complete the initialization the output variables to the value they should have
  for(v <- values.indices){
    clusters(values(v).valueInt).insertValue(v)
  }

  //This method is called by each IntVar that is registered to the dynamic dependency graph.
  //We update the output variables incrementally based on this update.
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Int, NewVal: Int): Unit = {
    assert(values(index) == v)
    clusters(OldVal).deleteValue(index)
    clusters(NewVal).insertValue(index)
  }

  //This method is optional, it is called by the model when its debug mode is activated (see the constructor of model)
  //In this method, we check that the outputs are correct, based on non-incremental code
  override def checkInternals(c:Checker): Unit ={
    for(v <- values.indices){
      c.check(clusters(values(v).valueInt).value.contains(v),
        Some(s"clusters(values(v ($v)).value (${values(v).value})).value.contains(v)"))
    }
    for(value <- clusters.indices){
      for (indices <- clusters(value).value){
        c.check(values(indices).value == value,
          Some(s"values(indices).value (${values(indices).value}) == value ($value)"))
      }
    }
  }
}

/**Maintains a cluster of the indexes of array: cluster(j) = {i in index of values | values[i] == j}
  * This is considered as a dense cluster because Cluster is an array and must cover all the possibles values of the values in the array ''values''
  * @author renaud.delandtsheer@cetic.be
  * */
case class TranslatedDenseCluster(values:Array[IntValue],  indicesArray:Array[Int], clusters:Array[CBLSSetVar]) extends Invariant with ShortIntNotificationTarget{

  //We register the static and dynamic dependencies.
  //Dynamic dependencies are the ones considered for the notifications.
  //Static dependencies are the ones considered for ordering the propagations
  for (v <- values.indices) registerStaticAndDynamicDependency(values(v),v)

  //This must be called once all static dependencies are registered
  //It must be called before the output dependencies are notified
  finishInitialization()

  //We then define the variable that we control
  //By theway, an initial value is set to each of them (SortedSet.empty)
  for(c <- clusters){
    c.setDefiningInvariant(this) //A variable can only have a single controlling invariant
    c.setValue(SortedSet.empty)
  }

  //We then complete the initialization the output variables to the value they should have
  for(v <- values.indices){
    clusters(values(v).valueInt).insertValue(indicesArray(v))
  }

  //This method is called by each IntVar that is registered to the dynamic dependency graph.
  //We update the output variables incrementally based on this update.
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Int, NewVal: Int): Unit = {
    assert(values(index) == v)
    clusters(OldVal).deleteValue(indicesArray(index))
    clusters(NewVal).insertValue(indicesArray(index))
  }

  //This method is optional, it is called by the model when its debug mode is activated (see the constructor of model)
  //In this method, we check that the outputs are correct, based on non-incremental code
  override def checkInternals(c:Checker): Unit ={
    for(v <- values.indices){
      c.check(clusters(values(v).valueInt).value.contains(indicesArray(v)),
        Some(s"clusters(values(v ($v)).value (${values(v).value})).value.contains(v)"))
    }
    for(value <- clusters.indices){
      for (indices1 <- clusters(value).value; indices = indicesArray(indices1)){
        c.check(values(indices).value == value,
          Some(s"values(indices).value (${values(indices).value}) == value ($value)"))
      }
    }
  }
}

/**This is a helper object for the [[DenseCluster]]
  * and [[SparseCluster]]
  * invariants.
  * @author renaud.delandtsheer@cetic.be
  * */
object Cluster{

  def makeSparse(values:Array[IntValue], clusters: Iterable[Int]):SparseCluster = {
    val m:Store = InvariantHelper.findModel(values)
    val Clusters:SortedMap[Int,CBLSSetVar] = clusters.foldLeft(SortedMap.empty[Int, CBLSSetVar])((acc,c) => acc + ((c,new CBLSSetVar(m,SortedSet.empty, values.indices.start to values.indices.end,"cluster_"+c))))
    SparseCluster(values,Clusters)
  }

  def makeDense(values:Array[IntValue]):DenseCluster = {
    val (themin,themax) = InvariantHelper.getMinMaxBoundsShort(values)
    assert(themin == 0, "dense clusters must start at zero")
    val m:Store = InvariantHelper.findModel(values)
    val Clusters:Array[CBLSSetVar] = (for(c <- 0 to themax) yield new CBLSSetVar(m,SortedSet.empty, Domain(values.indices.start, values.indices.end),"cluster_"+c)).toArray
    DenseCluster(values,Clusters)
  }

  def makeDenseAssumingMinMax(values:Array[IntValue],themin:Int,themax:Int):DenseCluster = {
    assert(themin == 0, "dense clusters must start at zero")
    val m:Store = InvariantHelper.findModel(values)
    val Clusters:Array[CBLSSetVar] = (for(c <- 0 to themax) yield new CBLSSetVar(m,SortedSet.empty, Domain(values.indices.start, values.indices.end),"cluster_"+c)).toArray
    DenseCluster(values,Clusters)
  }
}
