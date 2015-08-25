<?php
/**
 * This example shows you how to use `restclient.el` in a PHP file.
 *
 * With Emacs, this file should be opened with the `php-mode` major mode, not
 * with `restclient-mode`. To let `restclient` identify request we need to
 * define a *prefix* in the `restclient-common-prefix` variable.
 *
 * For example:
 *
 * ```lisp
 * (setq restclient-common-prefix "^ *\\* rc> *")
 * ```
 *
 * This variable can be defined on each file in a `Local Variables` section
 * or, for a whole project, in a `dir-locals.el` file. For this example, see
 * near the end of the file.
 *
 * This indicates that all lines containing this prefix will be considered by
 * `restclient` as part of a request. So, in this case, any PHP comment line
 * with an asterisk followed by " rc>", optionally followed by spaces, is
 * considered a `restclient` line of code.
 *
 * For example:
 *
 * rc> # Variables definition
 * rc> :wpedia := "https://en.wikipedia.org/w/api.php"
 * rc> :mwiki := "https://www.mediawiki.org/w/api.php"
 *
 * is considered the first `resclient` section with some variables defined
 * there. Check it with `M-x restclient-mark-current`.
 *
 * IMPORTANT NOTE: Please notice that every request, or `restclient`
 * *section*, **MUST** have a title in a commentary line starting with `#`.
 */

namespace Restclient\Example;

class WikipediaClient
{
    /**
     * Return a Wikipedia article.
     *
     * @param string $title Title of the requested article.
     *
     * Example:
     * rc> # Get API article
     * rc> GET :wpedia?action=query&titles=Application_programming_interface&format=json
     *
     * Issue the Emacs command
     *
     ```lisp
     * M-x restclient-http-send-current-stay-in-window
     * ```
     *
     * or, even easier, go to the end of this expression and press `C-x C-e`:
     * (restclient-http-send-current-stay-in-window)
     */
    public function getArticle($title)
    {
        // Here you PHP code ...
    }

    /**
     * Another example of restclient inside a major mode.
     *
     * rc> # Get REST Article
     * rc> GET :mwiki?action=query&prop=contributors&titles=Main_Page&format=json
     *
     * Send request: (restclient-http-send-current-stay-in-window)
     */
    public function getContributors($title)
    {
        // Here you PHP code ...
    }

    /**
     * Bind `restclient-http-send-current-stay-in-window` to a key.
     *
     * In case you want to send current requests with a key in every
     * programming major mode:
     *
     * ```lisp
     * (add-hook 'prog-mode-hook
     *           '(lambda()
     *              (local-set-key [f4] 'restclient-http-send-current-stay-in-window)))
     * ```
     *
     * Try it right now: go to the end of next expression and press `C-x C-e`:
     * (local-set-key [f4] 'restclient-http-send-current-stay-in-window)
     *
     * Example:
     * rc> # get images
     * rc> GET :wpedia?action=query&titles=Albert%20Einstein&prop=images&format=json
     *
     * With the cursor positioned below the line `# get images', send request
     * by pressing F4.
     */
    public function getImages($title)
    {
        // Here your PHP code ...
    }
}

// Local Variables:
// restclient-common-prefix: "^ *\\* rc> *"
// End:
