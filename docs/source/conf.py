import os
import sys
from sphinx.highlighting import PygmentsBridge
from pygments.formatters.latex import LatexFormatter
class CustomLatexFormatter(LatexFormatter):
  def __init__(self, **options):
    super(CustomLatexFormatter, self).__init__(**options)
    self.verboptions = r"formatcom=\footnotesize" 
PygmentsBridge.latex_formatter = CustomLatexFormatter

templates_path = ['_templates']
source_suffix = '.rst'
master_doc = 'index'
project = u'OptiML'
copyright = u'2016,Stanford PPL'
version = '0.1'
release = '0.1'
highlight_language = 'none'


#### HTML Info ###
html_theme = 'sphinx_rtd_theme'
html_static_path = ['_static']
htmlhelp_basename = 'OptiMLDoc'

### LaTex Info ###
latex_documents = [
  ('index', 'OptiML.tex', u'OptiML Documentation',
  u'Stanford PPL', 'manual'),
]
latex_elements = {
  'classoptions': ',openany,oneside',
  'babel': '\\usepackage[english]{babel}'
}

### Manpage Info ###
man_pages = [
  ('index', 'optiml', u'OptiML Documentation',
  [u'Stanford PPL'], 1)
]

### TexInfo ###
texinfo_documents = [
  ('index', 'OptiML', u'OptiML Documentation',
  u'Stanford PPL', 'OptiML', 'One line description of project.',
  'Miscellaneous'),
]
