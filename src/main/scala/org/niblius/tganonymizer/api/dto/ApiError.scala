package org.niblius.tganonymizer.api.dto

case class ApiError(ok: Boolean, error_code: Int, description: String)
    extends Exception(description)
