case class Database(tables: List[Table]) {
  // TODO 3.0
  override def toString: String = tables.map(_.toString).mkString("\n")

  // TODO 3.1
  def insert(tableName: String): Database = {
    if (tables.exists(t => t.name == tableName)) this
    else Database(tables.appended(Table(tableName, Nil)))
  }

  // TODO 3.2
  def update(tableName: String, newTable: Table): Database = {
    Database(tables.map(t => if (t.name == tableName) newTable else t))
  }
  // TODO 3.3
  def delete(tableName: String): Database = {
    // pastreaza toate tabelele, in afara de cel cu numele tableName
    Database(tables.filterNot(t => t.name == tableName))
  }

  // TODO 3.4
  def selectTables(tableNames: List[String]): Option[Database] = {
    if (tableNames.exists(t => !tables.exists(a => a.name == t))) None
    else Some(Database(tables.filter(t => tableNames.contains(t.name))))
  }

  // TODO 3.5
  // Implement indexing here
  def apply(index: Int): Table = {
    tables(index)
  }
}
