import Test.HUnit
import Data.Monoid
import Control.Monad
import MiniMarkdown

testWriteHTMLString :: (String, String) -> Test
testWriteHTMLString (inp, out) =
     TestCase $ assertEqual "" out $ writeHTMLString inp

main :: IO Counts
main = do
    let testcases = [("**foo**", "<strong>foo</strong>"),
           ("**foo**foo" , "<strong>foo</strong>foo"),
           ("**foo**foo*bar*" , "<strong>foo</strong>foo<em>bar</em>"),
           ("*foo*" , "<em>foo</em>"),
           ("* foo \n" , "<li>foo </li>"),
           ("Stuff" , "Stuff"),
           ("Stuff*foo*" , "Stuff<em>foo</em>"),
           ("Stuff*foo***bar**" , "Stuff<em>foo</em><strong>bar</strong>"),
           ("#Head\n\n`foo`" , "<h1>Head</h1>\n\n<code>foo</code>"),
           ("#Head\n~~~\nfoo~bar\n~~~`" , "<h1>Head</h1>\n\n<code>foo</code>"),
           ("#Head\n\n\tfoo\n\tfoo\n\n" , ""),
           ("#Head\nfoo\n    foo\n    foo\n\n", "<h1>Head</h1>\nfoo<pre><code>foo\nfoo</code></pre"),
           ("#Head\nStuff*foo***bar**" , "<h1>Head</h1>Stuff<em>foo</em><strong>bar</strong>"),
           ("#Head\n**foo***bar*" , "<h1>Head</h1>\n<strong>foo</strong><em>bar</em>"),
           ("# Head\n" , "<h1>Head</h1>\n"),
           ("## Head\n" , "<h2>Head</h2>\n"),
           ("### Head\n" , "<h3>Head</h3>\n"),
           ("#### SUBTOPIC1\n" , "<h4>SUBTOPIC1</h4>\n"),
           ("foo #### SUBTOPIC1\n" , "foo <h4>SUBTOPIC1</h4>\n"),
           ("# Head\n## Head\n" , "<h1>Head</h1>\n<h2>Head</h2>\n"),
           ("# Head\n## Head\n### Head\n" , "<h1>Head</h1>\n<h2>Head</h2>\n<h3>Head</h3>\n"),
           ("[foo](www.foo.com)" , "<a href=\"www.foo.com\">foo</a>"),
           ("`foo`", "<code>foo</code>"),
           ("~~~\nfoo\n~~~\n", "<p><code>foo\n</code></p>")]
    let tests = map testWriteHTMLString testcases
    runTestTT $ TestList tests
