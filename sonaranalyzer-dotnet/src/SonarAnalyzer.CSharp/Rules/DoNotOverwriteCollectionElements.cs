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
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using SonarAnalyzer.Common;
using SonarAnalyzer.Helpers;

namespace SonarAnalyzer.Rules.CSharp
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    [Rule(DiagnosticId)]
    public sealed class DoNotOverwriteCollectionElements : DoNotOverwriteCollectionElementsBase
    {
        private static readonly DiagnosticDescriptor rule =
            DiagnosticDescriptorBuilder.GetDescriptor(DiagnosticId, MessageFormat, RspecStrings.ResourceManager);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } = ImmutableArray.Create(rule);

        protected override void Initialize(SonarAnalysisContext context)
        {
            context.RegisterSyntaxNodeActionInNonGenerated(
                new ReplacementVerifierAssignment().GetAnalysisAction(rule),
                SyntaxKind.SimpleAssignmentExpression,
                SyntaxKind.InvocationExpression);
        }

        private class ReplacementVerifierAssignment : ReplacementVerifierNew<ElementAccessExpressionSyntax, InvocationExpressionSyntax>
        {
            protected override SyntaxNode GetIndexOrKey(SyntaxNode assignmentOrInvocation)
            {
                switch (assignmentOrInvocation.Kind())
                {
                    case SyntaxKind.InvocationExpression:
                        return ((InvocationExpressionSyntax)assignmentOrInvocation).ArgumentList.Arguments[0];

                    case SyntaxKind.SimpleAssignmentExpression:
                        var elementAccess = ((AssignmentExpressionSyntax)assignmentOrInvocation).Left as ElementAccessExpressionSyntax;
                        return elementAccess.ArgumentList.Arguments[0];

                    default:
                        return null;
                }
            }

            protected override SyntaxNode GetCollectionIdentifier(SyntaxNode assignmentOrInvocation)
            {
                switch (assignmentOrInvocation.Kind())
                {
                    case SyntaxKind.InvocationExpression:
                        return GetCollectionIdentifier((InvocationExpressionSyntax)assignmentOrInvocation);

                    case SyntaxKind.SimpleAssignmentExpression:
                        var elementAccess = ((AssignmentExpressionSyntax)assignmentOrInvocation).Left as ElementAccessExpressionSyntax;
                        return GetCollectionIdentifier(elementAccess);

                    default:
                        return null;
                }
            }

            protected override IEnumerable<SyntaxNode> GetPreviousCollectionSets(SyntaxNode assignmentOrInvocation, SyntaxNode collectionIdentifier) =>
                GetPreviousStatements(assignmentOrInvocation.FirstAncestorOrSelf<StatementSyntax>())
                    .OfType<ExpressionStatementSyntax>()
                    .Select(statement => statement.Expression)
                    .TakeWhile(IsSameCollection(collectionIdentifier));

            private Func<ExpressionSyntax, bool> IsSameCollection(SyntaxNode collectionIdentifier) =>
                expression =>
                {
                    return GetCollectionIdentifier(expression) is SyntaxNode identifier &&
                        identifier.IsEquivalentTo(collectionIdentifier);
                };

            private static SyntaxNode GetCollectionIdentifier(ElementAccessExpressionSyntax node) =>
                GetIdentifier(node.Expression.RemoveParentheses());

            private static SyntaxNode GetCollectionIdentifier(InvocationExpressionSyntax node)
            {
                return GetIdentifier(node.Expression.RemoveParentheses());
            }

            private static SyntaxNode GetIdentifier(ExpressionSyntax expression)
            {
                switch (expression?.Kind())
                {
                    case SyntaxKind.MemberBindingExpression:
                        return ((MemberBindingExpressionSyntax)expression).Name;

                    case SyntaxKind.SimpleMemberAccessExpression:
                        return ((MemberAccessExpressionSyntax)expression).Name;

                    case SyntaxKind.IdentifierName:
                        return expression;

                    default:
                        return null;
                }
            }
        }

        /// <summary>
        /// Returns all statements before the specified statement within the containing method.
        /// This method recursively traverses all parent blocks of the provided statement.
        /// </summary>
        private static IEnumerable<StatementSyntax> GetPreviousStatements(StatementSyntax statement)
        {
            var previousStatements = statement.Parent.ChildNodes()
                .OfType<StatementSyntax>()
                .TakeWhile(x => x != statement)
                .Reverse();

            return statement.Parent is StatementSyntax parentStatement
                ? previousStatements.Union(GetPreviousStatements(parentStatement))
                : previousStatements;
        }
    }
}
