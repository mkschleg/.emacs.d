(setq sql-connection-alist
      '((cedar-mysql (sql-product 'mariadb)
                     (sql-port 8000)
                     (sql-server "127.0.0.1")
                     (sql-user "mkschleg")
                     (sql-password "jlli=+CvSTw6rNqo"))))

(provide 'sql_db)
