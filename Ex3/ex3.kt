/*  Sahar Cohen 206824088 saharco@campus.technion.ac.il
    Yuval Nahon 206866832 yuval.nahon@campus.technion.ac.il */

import java.io.File

fun completeMe(input: ByteArray): ByteArray {
    val offset : Byte = 96
    val output:ByteArray = input
    for (x in input.indices){
        var int8 : Int = input.get(x).toInt()
        if(int8 in -128..-102) {
            output.set(x, (input.get(x) + offset).toByte())
        }
    }
    return output
}