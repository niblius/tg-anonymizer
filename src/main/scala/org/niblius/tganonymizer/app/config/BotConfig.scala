package org.niblius.tganonymizer.app.config

case class BotConfig(test: DatabaseConfig,
                     development: DatabaseConfig,
                     production: DatabaseConfig)
