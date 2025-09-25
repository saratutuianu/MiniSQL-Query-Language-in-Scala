import scala._

object Queries {
  def query_1(db: Database, ageLimit: Int, cities: List[String]): Option[Table] = {
    def aux(): FilterCond = {
      val filter1 = new FilterCond {
        def eval(r: Row): Option[Boolean] = {
          if (!r.contains("age")) None
          else if (r("age").toInt > ageLimit) Some(true)
          else Some(false)
        }
      }
      val filter2 = new FilterCond {
        def eval(r: Row): Option[Boolean] = {
          if (!r.contains("city")) None
          else if (cities.contains(r("city"))) Some(true)
          else Some(false)
        }
      }
      filter1 && filter2
    }
    Some(Table("name", db.tables.flatMap(x => x.filter(aux()).data)))
  }

  def query_2(db: Database, date: String, employeeID: Int): Option[Table] = {
    def aux(r: Row): Option[Boolean] = {
      if (!r.contains("employee_id") || !r.contains("date")) None
      else if (r("employee_id").toInt == employeeID) Some(false)
      else if (date.take(4).toInt == r("date").take(4).toInt) {
        if (date.slice(5, 7).toInt == r("date").slice(5, 7).toInt) {
          if (date.slice(8, 10).toInt < r("date").slice(8, 10).toInt) Some(true)
          else Some(false)
        } else if (date.slice(5, 7).toInt < r("date").slice(5, 7).toInt) Some(true)
        else Some(false)
      } else if (date.take(4).toInt < r("date").take(4).toInt) Some(true)
      else Some(false)
    }
    Some(Table("name", db.tables.flatMap(x => x.sort("cost", ascending = false).data.filter(r => aux(r).getOrElse(false)).map { row =>
      row.filter { case (key, _) => key == "order_id" || key == "cost" }
    })))
  }

  def query_3(db: Database, minCost: Int): Option[Table] = {
    def aux(r: Row): Option[Boolean] = {
      if (!r.contains("cost")) None
      else if (r("cost").toInt > minCost) Some(true)
      else Some(false)
    }
    Some(Table("name", db.tables.flatMap(x => x.sort("employee_id").data.filter(r => aux(r).getOrElse(false)).map { row =>
      row.filter { case (key, _) => key == "order_id" || key == "cost" || key == "employee_id" }
    })))
  }
}
