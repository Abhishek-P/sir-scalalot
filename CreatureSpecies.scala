package com.scalalot.sir.creatures

class CreatureSpecies(val name: String) {
  override def toString: String = s"The magical world of Scala contains creature of species $name"
}

//class Valagon extends CreatureSpecies {
//  var age: Int
//  override def toString: String = s"A species of giants that are born formed and never change till the day they die."
//}