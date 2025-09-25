type Row = Map[String, String]
type Tabular = List[Row]

case class Table (tableName: String, tableData: Tabular) {

  // TODO 1.0
  // Se extrag cheile din primul Row, fiind identice pentru toate Row-urile
  // toList, deoarece keys returneaza Iterable[String]
  def header: List[String] = tableData.head.keys.toList
  def data: Tabular = tableData
  def name: String = tableName


  // TODO 1.1
  override def toString: String = {
    def op(c: Row, acc: String): String = {
      val s = c.values.mkString(",")
      // se adauga linia formata la acumulator
      "|".concat(s).concat("\n").concat(acc)
    }
    val res = data.foldRight("")(op)
    // se adauga headerul si se elimina ultimul "\n"
    "|".concat(header.mkString(",")).concat("\n").
      concat(res).dropRight(1).stripMargin
  }

  // TODO 1.3
  def insert(row: Row): Table = {
    if (data.contains(row)) Table(name, data)
    // append la sfarsit
    else Table(name, data :+ row)
  }

  // TODO 1.4
  def delete(row: Row): Table = {
    // se pastreaza doar liniile care sunt diferite de row
    if (data.contains(row)) Table(name, data.filter(_ != row))
    else Table(name, data)
  }

  // TODO 1.5
  def sort(column: String, ascending: Boolean = true): Table = {
    // vom sorta cu MergeSort
    def merge(l1: List[Row], l2: List[Row]): List[Row] = {
      (l1, l2) match {
        case (l, Nil) => l
        case (Nil, l) => l
        case (x :: xs, y :: ys) =>
          // daca este in ordine crescatoare
          if (ascending) {
            if (x(column) > y(column)) y :: merge(l1, ys)
            else x :: merge(xs, l2)
          } else {
            if (x(column) > y(column)) x :: merge(xs, l2)
            else y :: merge(l1, ys)
          }
      }
    }

    // verificam daca exista coloana dupa care se face ordonarea
    if (header.contains(column)) {
      this.data match {
        case Nil => this
        // daca are un singur element
        case _ :: Nil => this
        case _ =>
          val mid = this.data.size / 2
          // se sorteaza dreapta si stanga
          val tableLeft = Table(name, this.data.take(mid)).sort(column, ascending)
          val tableRight = Table(name, this.data.drop(mid)).sort(column, ascending)
          // se combina rezultatul
          Table(name, merge(tableLeft.data, tableRight.data))
      }
    } else Table(name, data)
  }

  // TODO 1.6
  def select(columns: List[String]): Table = {
    // filtram fiecare row, ramanand doar cheile continute in lista columns
    Table(name, data.map(_.filter{case (k, _) => columns.contains(k)}))
  }

  // TODO 1.7
  // Construiti headerele noi concatenand numele tabelei la headere
  def cartesianProduct(otherTable: Table): Table = {
    // numele lui otherTable este concatenat cu headerele din el
    val newHeaderOther = otherTable.header.map(c => otherTable.name.concat(".").concat(c))
    // numele acestui tabel este concatenat cu headerele de aici
    val newHeaderThis = header.map(c => name.concat(".").concat(c))
    // se concateneaza cele 2
    val newHeader = newHeaderOther ++ newHeaderThis

    // pentru fiecare row din data, se itereaza prin toate row-urile din 
    // otherTable.data, se concateneaza si li se atribuie cheia corespunzatoare
    val newData = for {
      z <- data
      w <- otherTable.data
    } yield newHeader.zip(w.values ++ z.values).toMap

    Table("name", newData)
  }

  // TODO 1.8
  def join(other: Table)(col1: String, col2: String): Table = {
    if (other == null || this == null) {
      throw new IllegalArgumentException("Unul dintre tabele este null!")
    } else if (data.isEmpty) other
    else if (other.data.isEmpty) this
    else {
      val commonElements = for {
        a <- data
        b <- other.data
        if b(col2) == a(col1)
        // un map nu poate avea chei identice; daca se adauga o noua pereche (cheie, valoare)
        // si cheia exista deja in map, perechea existenta va fi suprascrisa, transformam in liste
      } yield a.toList ++ (b - col2).toList

      // groupBy returneaza perechi k -> v, v este o lista
      // eliminam k -> v, perechea este inlocuita cu valorile distincte concatenate
      val common = commonElements.distinct.map(_.groupBy(_._1).transform((k, v) => v.distinct.map(_._2).mkString(";")))
      val allKeys = data.head.keys ++ (other.data.head - col2).keys

      // elementele ce se afla doar in al doilea tabel
      val secondTableElements = for {
        b <- other.data
        if !data.exists(a => a(col1) == b(col2))
      } yield {
        // se adauga perechea in b, trebuie sa aiba cheia col1
        val bReplace = b + (col1 -> b(col2))
        // daca cheia nu se afla in row b, atunci ramane ""
        // astfel, se va lua doar cheia col1 din b
        allKeys.map(k => k -> bReplace.getOrElse(k, "")).toMap
      }

      // elementele ce se afla doar in primul tabel
      val firstTableElements = for {
        b <- data
        if !other.data.exists(a => a(col2) == b(col1))
      } yield allKeys.map(k => k -> b.getOrElse(k, "")).toMap

      Table("name", common ++ firstTableElements ++ secondTableElements)
    }
  }

  // TODO 2.3
  def filter(f: FilterCond): Table = {
    Table(name, data.filter(r => f.eval(r).getOrElse(false)))
  }

  // TODO 2.4
  def update(f: FilterCond, updates: Map[String, String]): Table = {
    // daca row-ul indeplineste conditia, se updateaza doar elementele
    // ce se afla si in updates, adica care trebuie schimbate
    Table(name, data.map(r => if (f.eval(r).get) r.map( (k, v) =>
      if (updates.contains(k)) k -> updates(k) else k -> v)
    else r))
  }

  // TODO 3.5
  // Implement indexing
  def apply(index: Int): Row = {
    data(index)
  }
}

object Table {
  // TODO 1.2
  def fromCSV(csv: String): Table = {
    val lines = csv.split("\n").toList
    val head = lines.head.split(",").toList

    def op(c: String, acc: List[Row]): List[Row] = {
      head.zip(c.split(",").toList).toMap :: acc
    }

    // fara prima linie, care contine headerele
    val tabular = lines.drop(1).foldRight(Nil)(op)

    Table("name", tabular)
  }

  // TODO 1.9
  def apply(name: String, s: String): Table = {
    Table(name, fromCSV(s).tableData)
  }
}