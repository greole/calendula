import Test.HUnit
import Data.Monoid
import Control.Monad
import MiniMarkdown

{- http://spec.commonmark.org/0.22/ -}


testWriteHTMLString :: (String, String) -> Test
testWriteHTMLString (inp, out) =
     TestCase $ assertEqual "" out $ writeHTMLString inp

main :: IO Counts
main = do
    let testcases = [
           -- test if ws at beginning of a line are cleaned preoperly
           (" a", "<p>a</p>"),
           ("  a", "<p>a</p>"),
           ("   a", "<p>a</p>"),
           ("  \tfoo\tbaz\t\tbim", "<pre><code>foo\tbaz\t\tbim</code></pre>"), -- fails
           -- continue with example 8
           ("***\n---\n___\n", "<hr /><hr /><hr />"),
           ("+++", "<p>+++</p>"),
           ("===", "<p>===</p>"),
           ("**\n--\n__\n", "<p>** -- __</p>"),
           (" ***\n  ***\n   ***", "<hr /><hr /><hr />"),
           ("    ***", "<pre><code>***</code></pre>"),
           (" - - - ", "<hr />"),
           (" *-*", "<p><em>-</em></p>"), -- continue at example 21
           ("**foo**", "<p><strong>foo</strong></p>"),
           -- ("**foo**foo*bar*" , "<strong>foo</strong>foo<em>bar</em>"),
           -- ("*foo*" , "<em>foo</em>"),
           -- ("* foo \n" , "<li>foo </li>"),
           -- ("Stuff" , "Stuff"),
           -- ("Stuff*foo*" , "Stuff<em>foo</em>"),
           -- ("Stuff*foo***bar**" , "Stuff<em>foo</em><strong>bar</strong>"),
           -- ("#Head\n\n`foo`" , "<h1>Head</h1>\n\n<code>foo</code>"),
           -- ("#Head\n~~~\nfoo~bar\n~~~`" , "<h1>Head</h1>\n\n<code>foo</code>"),
           -- ("#Head\n\n\tfoo\n\tfoo\n\n" , ""),
           -- ("#Head\nfoo\n    foo\n    foo\n\n", "<h1>Head</h1>\nfoo<pre><code>foo\nfoo</code></pre"),
           -- ("#Head\nStuff*foo***bar**" , "<h1>Head</h1>Stuff<em>foo</em><strong>bar</strong>"),
           -- ("#Head\n**foo***bar*" , "<h1>Head</h1>\n<strong>foo</strong><em>bar</em>"),
           -- ("# Head\n" , "<h1>Head</h1>\n"),
           -- ("## Head\n" , "<h2>Head</h2>\n"),
           -- ("### Head\n" , "<h3>Head</h3>\n"),
           -- ("#### SUBTOPIC1\n" , "<h4>SUBTOPIC1</h4>\n"),
           -- ("foo #### SUBTOPIC1\n" , "foo <h4>SUBTOPIC1</h4>\n"),
           -- ("# Head\n## Head\n" , "<h1>Head</h1>\n<h2>Head</h2>\n"),
           -- ("# Head\n## Head\n### Head\n" , "<h1>Head</h1>\n<h2>Head</h2>\n<h3>Head</h3>\n"),
           -- ("[foo](www.foo.com)" , "<a href=\"www.foo.com\">foo</a>"),
           -- ("`foo`", "<code>foo</code>"),
           ("~~~\nfoo\n~~~\n", "<pre><code>foo</code></pre>")]
    let tests = map testWriteHTMLString testcases
    runTestTT $ TestList tests
