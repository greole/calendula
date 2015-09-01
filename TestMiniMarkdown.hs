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
           ("_ _ _ _ a\n\na------\n\n---a---\n\n", "<p>_ _ _ _ a</p><p>a------</p><p>---a---</p>"),
           (" *-*" , "<p><em>-</em></p>"),
           ("Foo\n***\nbar", "<p>Foo</p>\n<hr />\n<p>bar</p>"), -- hrules can interrupt a paragraph
           ("# foo\n## foo\n### foo\n#### foo\n##### foo\n###### foo\n", "<h1><a name=\"foo\">foo</a></h1><h2><a name=\"foo\">foo</a></h2><h3><a name=\"foo\">foo</a></h3><h4><a name=\"foo\">foo</a></h4><h5><a name=\"foo\">foo</a></h5><h6><a name=\"foo\">foo</a></h6>"), 
           -- ("*foo*" , "<em>foo</em>"),
           -- ("* foo \n" , "<li>foo </li>"),
           -- ("Stuff" , "Stuff"),
           -- ("Stuff*foo*" , "Stuff<em>foo</em>"),
           -- ("Stuff*foo***bar**" , "Stuff<em>foo</em><strong>bar</strong>"),
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
