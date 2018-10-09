/*
 * SonarAnalyzer for .NET
 * Copyright (C) 2015-2018 SonarSource SA
 * mailto: contact AT sonarsource DOT com
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;
using SonarAnalyzer.Helpers;

namespace SonarAnalyzer.Rules
{
    public abstract class DoNotOverwriteCollectionElementsBase : SonarDiagnosticAnalyzer
    {
        protected const string DiagnosticId = "S4143";
        protected const string MessageFormat = "Verify this is the index/key that was intended; a value has already been set for it.";

        protected abstract class ReplacementVerifierNew<TAssignmentExpression, TIvocationExpression>
            where TAssignmentExpression : SyntaxNode
            where TIvocationExpression : SyntaxNode
        {
            public Action<SyntaxNodeAnalysisContext> GetAnalysisAction(DiagnosticDescriptor rule) =>
                context =>
                {
                    var collectionIdentifier = GetCollectionIdentifier(context.Node);
                    var indexOrKey = GetIndexOrKey(context.Node);

                    if (collectionIdentifier == null || indexOrKey == null)
                    {
                        return;
                    }

                    var previous = GetPreviousCollectionSets(context.Node, collectionIdentifier)
                        .FirstOrDefault(IsSameIndexOrKey(indexOrKey));
                    if (previous != null)
                    {
                        context.ReportDiagnosticWhenActive(
                            Diagnostic.Create(rule,
                                context.Node.GetLocation(),
                                additionalLocations: new[] { previous.GetLocation() }));
                    }
                };

            private Func<SyntaxNode, bool> IsSameIndexOrKey(SyntaxNode indexOrKey) =>
                syntaxNode => GetIndexOrKey(syntaxNode).IsEquivalentTo(indexOrKey);

            protected abstract IEnumerable<SyntaxNode> GetPreviousCollectionSets(SyntaxNode syntaxNode, SyntaxNode collectionIdentifier);

            protected abstract SyntaxNode GetIndexOrKey(SyntaxNode syntaxNode);

            protected abstract SyntaxNode GetCollectionIdentifier(SyntaxNode syntaxNode);
        }
    }
}
