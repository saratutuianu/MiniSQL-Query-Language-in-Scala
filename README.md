# MiniSQL – Query Language in Scala

## Overview  
This project implements a **mini query language inspired by SQL** in **Scala**, with a lightweight database engine built from scratch.  
The system allows performing operations both on **individual tables** and on **interactions between multiple tables**, combining them  
through advanced operations such as joins and filters.  

The goal is to simulate how a relational database works, while also exploring **object-oriented programming** and **functional programming** concepts in Scala.  

---

## Features  

### Table Operations  
- **Table representation** using case classes and immutable data structures (`List[Row]`).  
- **CSV support**: import/export tables from and to CSV format.  
- **Insert**: add new rows to an existing table.  
- **Delete**: remove rows matching a given condition.  
- **Sort**: order rows by a given column, ascending or descending.  
- **Select**: extract only a subset of columns from a table.  
- **Cartesian Product**: combine two tables by pairing all rows.  
- **Join**: merge tables on specific columns, with special handling for `NULL`/empty values.  

### Filters & Conditions  
- A rich **filtering system** (`FilterCond`) that allows composing complex queries:  
  - `Field`: conditions on a single column (with a predicate).  
  - `Compound`: logical composition of multiple conditions.  
  - `Not`, `And`, `Or`, `Equal` operators.  
  - `Any` and `All` conditions for grouped filters.  
- **Custom operators** in Scala (`&&`, `||`, `!`, `==`) for concise query syntax.  

### Database Management  
- **Database abstraction** with multiple tables.  
- Operations:  
  - `insert`: add a new table  
  - `update`: replace an existing table  
  - `delete`: remove a table  
  - `selectTables`: extract a subset of tables by name  
- **Indexing support**:  
  - Access rows in a `Table` using `(index)`  
  - Access tables in a `Database` by index  

### Queries  
Example queries that simulate real-world SQL use cases:  
1. Select customers over a certain age and living in specific cities.  
2. Select orders after a certain date not processed by a given employee.  
3. Select orders above a cost threshold, ordered by employee ID.  

---

## Technologies Used  

- **Scala** – main programming language  
- **Scalatest & Scalactic** – unit testing and expressive assertions  
- **sbt** – build tool for compilation, running, and testing  
   git clone https://github.com/username/MiniSQL-Scala.git
   cd MiniSQL-Scala
