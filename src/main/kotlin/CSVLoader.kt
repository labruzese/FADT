package com.fischerabruzese

import java.io.BufferedReader
import java.io.File
import java.io.FileNotFoundException
import java.io.FileReader
import java.util.*
import kotlin.collections.HashMap

data class CSV(val headers: List<String>, val categories: List<MutableList<String?>>)

fun loadFile(path: String): CSV {
    val file = File(path)
    if (!file.exists()) throw FileNotFoundException("$path was not found")

    val bufferedReader = BufferedReader(FileReader(file))
    val headers = bufferedReader.readLine()?.split(",") ?: emptyList()
    val categories = LinkedList( headers.map { LinkedList<String?>() })

    bufferedReader.useLines { lines ->
        lines.forEach { row ->
            row.split(",").forEachIndexed { colNum, item ->
                if(categories.size == colNum) {
                    categories.add(LinkedList())
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
    val newCategories = mutableListOf<MutableList<String?>>()
    for ((header, category) in csv.headers.zip(csv.categories)) {

        transformCategory(category, ::missingFilter)

        val filterType = when {
            header.contains(Regex("Grade|Q1")) -> ::gradeFilter
            header.contains("Teacher") -> ::teacherFilter
            header.contains("Level") -> ::levelFilter
            header.contains("World Language") -> ::wlFilter
            header.contains("LP") -> ::lpFilter
            header.contains("Birth Month") -> ::dobFilter
            header.contains("Siblings") -> ::siblingFilter
            else -> {it -> it}
        }

        transformCategory(category, filterType)
    }

    return CSV(csv.headers, newCategories)
}

private fun transformCategory(items: MutableList<String?>, transform: (String) -> String?){
    for ((id, item) in items.withIndex()) {
        items[id] = item?.let { transform(it) }
    }
}

private fun missingFilter(item: String): String? {
    return if (item.equals("Missing", true))
        null
    else item
}

private fun siblingFilter(item: String): String? {
    return when {
        item.toIntOrNull() != null -> item
        else -> null
    }
}

private fun dobFilter(item: String): String? {
    return when {
        item.toIntOrNull() != null && (0..12).contains(item.toInt()) -> item
        else -> null
    }
}

private fun lpFilter(item: String): String? {
    return when (item) {
        "Yes", "No" -> item
        else -> null
    }
}

private fun wlFilter(item: String): String? {
    return when (item) {
        "Spanish", "French", "Chinese" -> item
        else -> null
    }
}

private fun levelFilter(item: String): String {
    return item
}

private val prevTeachers = HashMap<String, Int>()
private fun teacherFilter(item: String): String {
    val teacherNum = prevTeachers.putIfAbsent(item, prevTeachers.size) ?: (prevTeachers.size - 1)

    return "Teacher $teacherNum"
}

private fun gradeFilter(item: String): String? {
    var newItem = item

    newItem = newItem.removeSuffix("+")
    newItem = newItem.removeSuffix("-")

    if(newItem.length != 1) return null

    return newItem
}
