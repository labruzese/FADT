package com.fischerabruzese

import java.io.BufferedReader
import java.io.File
import java.io.FileNotFoundException
import java.io.FileReader
import java.util.*

data class CSV(val headers: List<String>, val categories: List<List<String?>>)

fun loadFile(path: String): CSV {
    val file = File(path)
    if (!file.exists()) throw FileNotFoundException("$path was not found")

    val bufferedReader = BufferedReader(FileReader(file))
    val headers = bufferedReader.readLine()?.split(",") ?: emptyList()
    val categories = LinkedList( headers.map { emptyList<String?>() })

    bufferedReader.useLines { lines ->
        lines.forEach { row ->
            row.split(",").forEachIndexed { colNum, item ->
                if(categories.size == colNum) {
                    categories.add(emptyList())
                    headers.addLast("no_name")
                }

                categories[colNum].addFirst(item)
            }
        }
    }
    bufferedReader.close()

    return formatCSV(CSV(headers, categories))
}

private fun formatCSV(csv: CSV): CSV {
    val newCategories = mutableListOf<List<String?>>()
    for ((header, category) in csv.headers.zip(csv.categories)) {
        when(header) {
            else -> newCategories.add(category)
        }
    }

    return CSV(csv.headers, newCategories)
}