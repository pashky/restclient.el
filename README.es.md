# restclient.el

Esta es una herramienta para explorar y probar manualmente servicios web
HTTP REST.
Ejecuta peticiones desde un documento de peticiones en texto plano, muestra
los resultados como XML, JSON formateados e incluso imágenes.

![](http://i.imgur.com/QtCID.png)

# Uso

Puedes instalar fácilmente `restclient` desde
[MELPA](http://melpa.org/) `package.el`.

Alternativamente, puesde colocar `restclient.el` donde tengas normalmente
tus ficheros lisp y añadir `(require restclient)` en tu fichero de inicio de
Emacs.

Una vez instalado, puedes preparar el fichero de texto con las peticiones.

`restclient-mode` es un modo mayor que resalta ligeramente la sintaxis y
soporta algunos atajos de teclado tradicionales.

- `C-c C-c`: Ejecuta la petición en la que se encuentra el cursor.
Formatea la respuesta cuando es posible
- `C-c C-r`: lo mismo pero sin modificar la respuesta
- `C-c C-v`: lo mismo que `C-c C-c`, pero no cambia el foco a otra ventana
- `C-c C-p`: salta a la petición anterior
- `C-c C-n`: salta a la siguiente petición
- `C-c C-.`: marca la petición en la que está el cursor
- `C-c C-u`: copia la petición en la que está el cursor a comando curl
- `C-c C-g`: inicia sesión [helm](https://emacs-helm.github.io/helm/) con
fuentes para variables y peticiones (en el caso de que helm esté disponible,
por supuesto)
- `C-c n n`: reduce a la región de la petición actual (incluye las cabeceras)
- `TAB`: oculta/muestra el cuerpo de la petición actual
- `C-c C-a`: muestra todas las partes contraídas
- `C-c C-i`: muestra información sobre las variables de restclient en el punto

Las dos últimas funciones están implementadas como `restclient-outline-mode`
modo menor, el cual está activado por omisión como hook del modo mayor.
Elimina este hook con `(remove-hook 'restclient-mode-hook
'restclient-outline-mode)` en el caso que no quieras este comportamiento
o si colisiona con otro atajo de teclado para `TAB` como auto completar.

Ejemplo de fichero de peticiones:

    # -*- restclient -*-
    #
    # Obtiene todas las APIs de Github, formatea el JSON de respuesta,
	# muestra el el estado y las cabeceras de la respuesta al final.
    # Además envía la cabecera User-Agent ya que la API de Github la requiere.
    #
    GET https://api.github.com
    User-Agent: Emacs Restclient

    #
    # Compatible con XML - resaltado, formateo.
    #
    GET http://www.redmine.org/issues.xml?limit=10

    #
    # ¡Puede, incluso, mostrar imágenes!
    #
    GET http://upload.wikimedia.org/wikipedia/commons/6/63/Wikipedia-logo.png
    #
    # Un poco de json GET, puedes enviar cabeceras también
    #
    GET http://jira.atlassian.com/rest/api/latest/issue/JRA-9
    User-Agent: Emacs24
    Accept-Encoding: compress, gzip

    #
    # Post también funciona, la entidad simplemente va tras una línea en blanco.
	# Lo mismo aplica a PUT.
    #
    POST https://jira.atlassian.com/rest/api/2/search
    Content-Type: application/json

    {
            "jql": "project = HCPUB",
            "startAt": 0,
            "maxResults": 15,
            "fields": [
                    "summary",
                    "status",
                    "assignee"
            ]
    }
    #
    # Y DELETE, responderá con un not-found error...
    #
    DELETE https://jira.atlassian.com/rest/api/2/version/20

    # Asigna a variable el valor de tu dirección ip utilizando una expresión jq
    GET http://httpbin.org/ip
    -> jq-set-var :my-ip .origin

Las lineas que comienzan con `#` son consideradas comentarios Y también
separadores de peticiones.

HTTPS y muestra imágenes requieren dlls adicionales en windows (libtls, libpng,
libjpg etc), los cuales no están en la distribución de emacs.

Más ejemplos en la carpeta `examples`.

# Variables en el buffer

Puedes declarar una variable como sigue:

    :myvar = the value

O así:

    :myvar := (some (artbitrary 'elisp)

De la segunda forma, el valor d la variable es evaluado como
Emacs Lisp inmediatamente. La evaluación de las variables es arriba a bajo.
Solo una expresión de una línea es permitida por variable, por lo que utiliza
`(progn ...)` y envolvente virtual de línea en caso de necesitar más variables
por línea. No hay forma de referenciar variables _restclient_ declaradas
previamente, pero siempre puedes usar `setq` para guardar el estado.

Las variables pueden ser multi-línea también:

    :myvar = <<
    Authorization: :my-auth
    Content-Type: application/json
    User-Agent: SomeApp/1.0
    #

o

    :myvar := <<
    (some-long-elisp
        (code spanning many lines)
    #

`<<` se utiliza para identificar el inicio de un valor multi-línea,
empezando el valor en la siguiente línea. El final del valor de la variable
es el carácter de comentario `#` y el ultimo final de línea no cuenta, como en
el cuerpo de las peticiones.

Una vez declarada la variable, la puedes usar en la URL, valores de la
cabecera y el cuerpo.

    # Some generic vars

    :my-auth = 319854857345898457457
    :my-headers = <<
    Authorization: :my-auth
    Content-Type: application/json
    User-Agent: SomeApp/1.0
    #

    # Update a user's name

    :user-id = 7
    :the-name := (format "%s %s %d" 'Neo (md5 "The Chosen") (+ 100 1))

    PUT http://localhost:4000/users/:user-id/
    :my-headers

    { "name": ":the-name" }

Las variables pueden también ser establecidas en el cuerpo de la
respuesta utilizando hooks por petición.

    # establece la variable :my-ip con el valor de tu dirección ip evaluando
	# elisp en el buffer del resultado
    GET http://httpbin.org/ip
    -> run-hook (restclient-set-var ":my-ip" (cdr (assq 'origin (json-read))))

    # igual con jq si está instalado
    GET http://httpbin.org/ip
    -> jq-set-var :my-ip .origin

    # establece la variable :my-var utilizando una expresión jq más compleja
	# (requiere jq-mode)
    GET https://httpbin.org/json
    -> jq-set-var :my-var .slideshow.slides[0].title

    # los hooks se establecen antes del body en POST
    POST http://httpbin.org/post
    -> jq-set-var :test .json.test

    {"test": "foo"}

# Subir ficheros

Restclient ahora permite especificar la ruta del fichero para usar como cuerpo,
de la siguiente forma:

    POST http://httpbin.org/post
    Content-type: text/plain

    < /etc/passwd

### A tener en cuenta:

- Variables multi-linea pueden utilizarse en la cabecera o en el cuerpo.
En la URL también pero no tiene mucho sentido a no ser que se trate de una
expresión compleja elisp que se evalúe a un valor simple.
- Aunque la misma variable no se puede usar en la cabecera y en el cuerpo,
tiene que ser dividida en dos y separadas por una línea en blanco como
es habitual.
- Variables ahora pueden referenciarse entre ellas, ya que la sustitución
ocurre en varias fases y se detiene cuando no no hay más variables.
Por favor, evita referencias circulares. Aunque hay un límite de seguridad
configurable que por omisión tiene 10 fases máximo para evitar cuelgues por
este motivo, hará que el proceso sea más lento.
- Declaración de variables solo se contempla encima de la línea de la petición.
- Cuidado con el elisp que incorporas. Ninguna validación de seguridad se
lleva a cabo por lo que podría formatear tu disco duro. En caso de que exista
un error de parseo o evaluación, será mostrado en el minibuffer.
- Las variables elisp pueden evaluarse a valores que contienen otras
referencias de variables. Estás serán reemplazadas también.
Pero no podrás remplazar partes de expresiones elips.

# Personalización

Hay varias variables para personalizar a tu gusto en `restclient`. También las
fuentes son personalizables en el grupo `restclient-faces`.

### restclient-log-request

__Default: t__

Determina si restclient escribe logs en el buffer \*Messages\* o no.

Si no-nil, las peticiones de restclient quedarán registradas en el buffer
\*Messages\*. Si nil, estas peticiones no quedarán registradas.

### restclient-same-buffer-response

__Default: t__

Reutilizar el mismo buffer para las respuestas o crear un buffer nuevo en
cada petición.

Si no-nil, reutiliza el buffer con el nombre `rest-client-buffer-response-name`
para todas las peticiones.

Si nil, genera un buffer con el nombre en función del tipo de petición y url
y un índice incremental por cada petición sucesiva.

Por ejemplo, `GET http://example.org` resultará en los siguientes nombres de
buffer en 3 peticiones consecutivas:
- `*HTTP GET http://example.org*`
- `*HTTP GET http://example.org*<2>`
- `*HTTP GET http://example.org*<3>`

### restclient-same-buffer-response-name

__Default: \*HTTP Response\*__

Nombre del buffer de respuesta a usar cuando `restclient-same-buffer-response`
es true.

### restclient-inhibit-cookies

__Default: nil__

Deshabilita el envío implícito de cookies por restclient.

# Problemas conocidos

- Líneas comentadas `#` sirve también como terminador de entidad.
Efectivamente, esto significa que no puedes usar post para enviar scripts de
shell o cualquier otra cosa con almohadillas como entidad en PUT/POST.
Por el momento me parece bien pero podría usar algún separador único en el
futuro.
- No estoy seguro si diferentes encodings son soportados.
Sospecho que non-ascii no funcionará. Aún tengo averiguarlo.
- El uso de variables no está resaltado.
- Si tu versión de Emacs es anterior a 26.1, algunas peticiones GET a
`localhost` podrían fallar por este
[bug](http://debbugs.gnu.org/cgi/bugreport.cgi?bug=17976) en Emacs/url.el.
Como solución alternativa puedes usar `127.0.0.0` en lugar de `localhost`

# Histórico

- _01/Ago/2016_ Añade la capacidad de reducir regiones
- _06/Apr/2016_ Fuentes Helm para variables y peticiones añadidas.
- _06/Apr/2016_ ¡Subida de ficheros! Mira arriba para consultar la sintaxis.
- _06/Apr/2016_ Añade fuentes personalizables para todo el resaltado de sintaxis.
- _05/Apr/2016_ Añade capacidad para declarar variables multi-línea (por
ejemplo un conjunto de cabeceras repetidos para cada petición) y
sustitución de valores de variables recursivamente.
- _25/Mar/2015_ Corta la última nueva línea del cuerpo de la petición.
- _15/Jun/2013_ Añade soporte para variables.

# Paquetes de 3ros relacionados

- [company-restclient](https://github.com/iquiw/company-restclient): Provee
auto-completado para métodos HTTP y cabeceras en restclient-mode. La fuente
del completado es ofrecida por know-your-http-well.
- [ob-restclient](https://github.com/alf/ob-restclient.el): Extensión de
restclient.el para emacs que ofrece soporte para org-babel.
- [restclient.vim](https://github.com/bounceme/restclient.vim): ¡Restclienten
vim! Muestra las respuestas en el paginador interno de vim.

# Licencia

Dominio público, haz lo que quieras.

# Autor

Pavel Kurnosov <pashky@gmail.com>
